;;; projectile.el --- Manage and navigate projects in Emacs easily -*- lexical-binding: t -*-

;; Copyright © 2011-2017 Bozhidar Batsov <bozhidar@batsov.com>

;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/projectile
;; Package-Version: 20171009.848
;; Keywords: project, convenience
;; Version: 0.15.0-cvs
;; Package-Requires: ((emacs "24.1") (pkg-info "0.4"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides easy project management and navigation.  The
;; concept of a project is pretty basic - just a folder containing
;; special file.  Currently git, mercurial and bazaar repos are
;; considered projects by default.  If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it.  See the README for more details.
;;
;;; Code:

(require 'cl-lib)
(require 'thingatpt)
(require 'ibuffer)
(require 'ibuf-ext)
(require 'compile)
(require 'grep)

(eval-when-compile
  (defvar ag-ignore-list)
  (defvar ggtags-completion-table)
  (defvar tags-completion-table)
  (defvar tags-loop-scan)
  (defvar tags-loop-operate)
  (defvar eshell-buffer-name)
  (defvar explicit-shell-file-name))

(declare-function ggtags-ensure-project "ggtags")
(declare-function ggtags-update-tags "ggtags")
(declare-function pkg-info-version-info "pkg-info")
(declare-function tags-completion-table "etags")
(declare-function make-term "term")
(declare-function term-mode "term")
(declare-function term-char-mode "term")

(defvar grep-files-aliases)
(defvar grep-find-ignored-directories)
(defvar grep-find-ignored-files)

;;;; Compatibility
(eval-and-compile
  ;; Added in Emacs 24.3.
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      `(progn
         (defvar ,var ,val ,docstring)
         (make-variable-buffer-local ',var))))

  ;; Added in Emacs 24.4
  (unless (fboundp 'string-suffix-p)
    (defun string-suffix-p (suffix string &optional ignore-case)
      "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
      (let ((start-pos (- (length string) (length suffix))))
        (and (>= start-pos 0)
             (eq t (compare-strings suffix nil nil
                                    string start-pos nil ignore-case))))))

  ;; Improved (no more stack overflows) in Emacs 24.5
  (eval-after-load 'etags
    '(when (< emacs-major-version 25)
       (defvar etags--table-line-limit 500)
       (defun etags-tags-completion-table ()
         (let ((table (make-vector 511 0))
               (progress-reporter
                (make-progress-reporter
                 (format "Making tags completion table for %s..." buffer-file-name)
                 (point-min) (point-max))))
           (save-excursion
             (goto-char (point-min))
             (while (not (eobp))
               (if (not (re-search-forward
                         "[\f\t\n\r()=,; ]?\177\\\(?:\\([^\n\001]+\\)\001\\)?"
                         (+ (point) etags--table-line-limit) t))
                   (forward-line 1)
                 (intern (prog1 (if (match-beginning 1)
                                    (buffer-substring (match-beginning 1) (match-end 1))
                                  (goto-char (match-beginning 0))
                                  (skip-chars-backward "^\f\t\n\r()=,; ")
                                  (prog1
                                      (buffer-substring (point) (match-beginning 0))
                                    (goto-char (match-end 0))))
                           (progress-reporter-update progress-reporter (point)))
                         table))))
           table)))))

(defun projectile-trim-string (string)
  "Remove whitespace at the beginning and end of STRING."
  (replace-regexp-in-string
   "[ 	\n\r]+\\'"
   ""
   (replace-regexp-in-string
    "\\`[ 	\n\r]+"
    ""
    string)))


;;; Customization
(defgroup projectile nil
  "Manage and navigate projects easily."
  :group 'tools
  :group 'convenience
  :link '(url-link :tag "Github" "https://github.com/bbatsov/projectile")
  :link '(url-link :tag "Online Manual" "https://projectile.readthedocs.org")
  :link '(emacs-commentary-link :tag "Commentary" "projectile"))

(defcustom projectile-indexing-method (if (eq system-type 'windows-nt) 'native 'alien)
  "Specifies the indexing method used by Projectile.

There are two indexing methods - native and alien.

The native method is implemented in Emacs Lisp (therefore it is
native to Emacs).  Its advantage is that it is portable and will
work everywhere that Emacs does.  Its disadvantage is that it is a
bit slow (especially for large projects).  Generally it's a good
idea to pair the native indexing method with caching.

The alien indexing method uses external tools (e.g. git, find,
etc) to speed up the indexing process.  The disadvantage of this
method is that it's not well supported on Windows systems.

By default alien indexing is the default on all operating
systems, except Windows."
  :group 'projectile
  :type '(radio
          (const :tag "Native" native)
          (const :tag "Alien" alien)))

(defcustom projectile-enable-caching (eq projectile-indexing-method 'native)
  "When t enables project files caching.

Project caching is automatically enabled by default if you're
using the native indexing method."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-file-exists-local-cache-expire nil
  "Number of seconds before file existence cache expires for a
file on a local file system.

 A value of nil disables this cache."

  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom projectile-file-exists-remote-cache-expire (* 5 60)
  "Number of seconds before file existence cache expires for a
file on a remote file system such as tramp.

 A value of nil disables this cache."
  :group 'projectile
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds")))

(defcustom projectile-require-project-root t
  "Require the presence of a project root to operate when true.
Otherwise consider the current directory the project root."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-completion-system 'ido
  "The completion system to be used by Projectile."
  :group 'projectile
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Grizzl" grizzl)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)
          (function :tag "Custom function")))

(defcustom projectile-keymap-prefix (kbd "C-c p")
  "Projectile keymap prefix."
  :group 'projectile
  :type 'string)

(defcustom projectile-cache-file
  (expand-file-name "projectile.cache" user-emacs-directory)
  "The name of Projectile's cache file."
  :group 'projectile
  :type 'string)

(defcustom projectile-build-dir "build"
  "The directory Projectile will use for build systems that build out of tree."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-file-name "TAGS"
  "The tags filename Projectile's going to use."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-command "ctags -Re -f \"%s\" %s"
  "The command Projectile's going to use to generate a TAGS file."
  :group 'projectile
  :type 'string)

(defcustom projectile-tags-backend 'auto
  "The tag backend that Projectile should use.

If set to 'auto', `projectile-find-tag' will automatically choose
which backend to use.  Preference order is ggtags -> xref
-> etags-select -> `find-tag'.  Variable can also be set to specify which
backend to use.  If selected backend is unavailable, fall back to
`find-tag'.

If this variable is set to 'auto' and ggtags is available, or if
set to 'ggtags', then ggtags will be used for
`projectile-regenerate-tags'.  For all other settings
`projectile-tags-command' will be used."
  :group 'projectile
  :type '(radio
          (const :tag "auto" auto)
          (const :tag "xref" xref)
          (const :tag "ggtags" ggtags)
          (const :tag "etags" etags-select)
          (const :tag "standard" find-tag))
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-sort-order 'default
  "The sort order used for a project's files."
  :group 'projectile
  :type '(radio
          (const :tag "default" default)
          (const :tag "recentf" recentf)
          (const :tag "recently active" recently-active)
          (const :tag "access time" access-time)
          (const :tag "modification time" modification-time)))

(defcustom projectile-verbose t
  "Echo messages that are not errors."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-buffers-filter-function nil
  "A function used to filter the buffers in `projectile-project-buffers'.

The function should accept and return a list of Emacs buffers.
Two example filter functions are shipped by default -
`projectile-buffers-with-file' and
`projectile-buffers-with-file-or-process'."
  :group 'projectile
  :type 'function)

(defcustom projectile-project-name nil
  "If this value is non-nil, it will be used as project name.

It has precedence over function `projectile-project-name-function'."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-project-name-function 'projectile-default-project-name
  "A function that receives the project-root and returns the project name.

If variable `projectile-project-name' is non-nil, this function will not be used."
  :group 'projectile
  :type 'function
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-project-root-files
  '("rebar.config"       ; Rebar project file
    "project.clj"        ; Leiningen project file
    "build.boot"         ; Boot-clj project file
    "SConstruct"         ; Scons project file
    "pom.xml"            ; Maven project file
    "build.sbt"          ; SBT project file
    "gradlew"            ; Gradle wrapper script
    "build.gradle"       ; Gradle project file
    ".ensime"            ; Ensime configuration file
    "Gemfile"            ; Bundler file
    "requirements.txt"   ; Pip file
    "setup.py"           ; Setuptools file
    "tox.ini"            ; Tox file
    "composer.json"      ; Composer project file
    "Cargo.toml"         ; Cargo project file
    "mix.exs"            ; Elixir mix project file
    "stack.yaml"         ; Haskell's stack tool based project
    "info.rkt"           ; Racket package description file
    "DESCRIPTION"        ; R package description file
    "TAGS"               ; etags/ctags are usually in the root of project
    "GTAGS"              ; GNU Global tags
    )
  "A list of files considered to mark the root of a project.
The topmost match has precedence."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-project-root-files-bottom-up
  '(".projectile" ; projectile project marker
    ".git"        ; Git VCS root dir
    ".hg"         ; Mercurial VCS root dir
    ".fslckout"   ; Fossil VCS root dir
    "_FOSSIL_"    ; Fossil VCS root DB on Windows
    ".bzr"        ; Bazaar VCS root dir
    "_darcs"      ; Darcs VCS root dir
    )
  "A list of files considered to mark the root of a project.
The bottommost (parentmost) match has precedence."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-project-root-files-top-down-recurring
  '(".svn" ; Svn VCS root dir
    "CVS"  ; Csv VCS root dir
    "Makefile")
  "A list of files considered to mark the root of a project.
The search starts at the top and descends down till a directory
that contains a match file but its parent does not.  Thus, it's a
bottommost match in the topmost sequence of directories
containing a root file."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-project-root-files-functions
  '(projectile-root-local
    projectile-root-bottom-up
    projectile-root-top-down
    projectile-root-top-down-recurring)
  "A list of functions for finding project roots."
  :group 'projectile
  :type '(repeat function))

(defcustom projectile-globally-ignored-files
  (list projectile-tags-file-name)
  "A list of files globally ignored by projectile."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-unignored-files nil
  "A list of files globally unignored by projectile."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-globally-ignored-file-suffixes
  nil
  "A list of file suffixes globally ignored by projectile."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-ignored-directories
  '(".idea"
    ".ensime_cache"
    ".eunit"
    ".git"
    ".hg"
    ".fslckout"
    "_FOSSIL_"
    ".bzr"
    "_darcs"
    ".tox"
    ".svn"
    ".stack-work")
  "A list of directories globally ignored by projectile."
  :safe (lambda (x) (not (remq t (mapcar #'stringp x))))
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-unignored-directories nil
  "A list of directories globally unignored by projectile."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-globally-ignored-modes
  '("erc-mode"
    "help-mode"
    "completion-list-mode"
    "Buffer-menu-mode"
    "gnus-.*-mode"
    "occur-mode")
  "A list of regular expressions for major modes ignored by projectile.

If a buffer is using a given major mode, projectile will ignore
it for functions working with buffers."
  :group 'projectile
  :type '(repeat string))

(defcustom projectile-globally-ignored-buffers nil
  "A list of buffer-names ignored by projectile.

If a buffer is in the list projectile will ignore
it for functions working with buffers."
  :group 'projectile
  :type '(repeat string)
  :package-version '(projectile . "0.12.0"))

(defcustom projectile-find-file-hook nil
  "Hooks run when a file is opened with `projectile-find-file'."
  :group 'projectile
  :type 'hook)

(defcustom projectile-find-dir-hook nil
  "Hooks run when a directory is opened with `projectile-find-dir'."
  :group 'projectile
  :type 'hook)

(defcustom projectile-switch-project-action 'projectile-find-file
  "Action invoked after switching projects with `projectile-switch-project'.

Any function that does not take arguments will do."
  :group 'projectile
  :type 'function)

(defcustom projectile-find-dir-includes-top-level nil
  "If true, add top-level dir to options offered by `projectile-find-dir'."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-use-git-grep nil
  "If true, use `vc-git-grep' in git projects."
  :group 'projectile
  :type 'boolean)

(defcustom projectile-grep-finished-hook nil
  "Hooks run when `projectile-grep' finishes."
  :group 'projectile
  :type 'hook
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-test-prefix-function 'projectile-test-prefix
  "Function to find test files prefix based on PROJECT-TYPE."
  :group 'projectile
  :type 'function)

(defcustom projectile-test-suffix-function 'projectile-test-suffix
  "Function to find test files suffix based on PROJECT-TYPE."
  :group 'projectile
  :type 'function)


;;; Idle Timer
(defvar projectile-idle-timer nil
  "The timer object created when `projectile-enable-idle-timer' is non-nil.")

(defcustom projectile-idle-timer-seconds 30
  "The idle period to use when `projectile-enable-idle-timer' is non-nil."
  :group 'projectile
  :type 'number)

(defcustom projectile-idle-timer-hook '(projectile-regenerate-tags)
  "The hook run when `projectile-enable-idle-timer' is non-nil."
  :group 'projectile
  :type '(repeat symbol))

(defcustom projectile-enable-idle-timer nil
  "Enables idle timer hook `projectile-idle-timer-functions'.

When `projectile-enable-idle-timer' is non-nil, the hook
`projectile-idle-timer-hook' is run each time Emacs has been idle
for `projectile-idle-timer-seconds' seconds and we're in a
project."
  :group 'projectile
  :set (lambda (symbol value)
         (set symbol value)
         (when projectile-idle-timer
           (cancel-timer projectile-idle-timer))
         (setq projectile-idle-timer nil)
         (when projectile-enable-idle-timer
           (setq projectile-idle-timer (run-with-idle-timer
                                        projectile-idle-timer-seconds t
                                        (lambda ()
                                          (when (projectile-project-p)
                                            (run-hooks 'projectile-idle-timer-hook)))))))
  :type 'boolean)

;;; Serialization
(defun projectile-serialize (data filename)
  "Serialize DATA to FILENAME.

The saved data can be restored with `projectile-unserialize'."
  (when (file-writable-p filename)
    (with-temp-file filename
      (insert (let (print-length) (prin1-to-string data))))))

(defun projectile-unserialize (filename)
  "Read data serialized by `projectile-serialize' from FILENAME."
  (with-demoted-errors
      "Error during file deserialization: %S"
    (when (file-exists-p filename)
      (with-temp-buffer
        (insert-file-contents filename)
        ;; this will blow up if the contents of the file aren't
        ;; lisp data structures
        (read (buffer-string))))))

(defvar projectile-projects-cache nil
  "A hashmap used to cache project file names to speed up related operations.")

(defvar projectile-project-root-cache (make-hash-table :test 'equal)
  "Cached value of function `projectile-project-root`.")

(defvar projectile-project-type-cache (make-hash-table :test 'equal)
  "A hashmap used to cache project type to speed up related operations.")

(defvar projectile-known-projects nil
  "List of locations where we have previously seen projects.
The list of projects is ordered by the time they have been accessed.

See also `projectile-remove-known-project',
`projectile-cleanup-known-projects' and `projectile-clear-known-projects'.")

(defvar projectile-known-projects-on-file nil
  "List of known projects reference point.

Contains a copy of `projectile-known-projects' when it was last
synchronized with `projectile-known-projects-file'.")

(defcustom projectile-known-projects-file
  (expand-file-name "projectile-bookmarks.eld"
                    user-emacs-directory)
  "Name and location of the Projectile's known projects file."
  :group 'projectile
  :type 'string)

(defcustom projectile-ignored-projects nil
  "A list of projects not to be added to `projectile-known-projects'."
  :group 'projectile
  :type '(repeat :tag "Project list" directory)
  :package-version '(projectile . "0.11.0"))

(defcustom projectile-ignored-project-function nil
  "Function to decide if a project is added to `projectile-known-projects'.

Can be either nil, or a function that takes the truename of the
project root as argument and returns non-nil if the project is to
be ignored or nil otherwise.

This function is only called if the project is not listed in
`projectile-ignored-projects'.

A suitable candidate would be `file-remote-p' to ignore remote
projects."
  :group 'projectile
  :type '(choice
          (const :tag "Nothing" nil)
          (const :tag "Remote files" file-remote-p)
          function)
  :package-version '(projectile . "0.13.0"))

(defcustom projectile-track-known-projects-automatically t
  "Controls whether Projectile will automatically register known projects.

When set to nil you'll have always add projects explicitly with
`projectile-add-known-project'."
  :group 'projectile
  :type 'boolean
  :package-version '(projectile . "0.15.0"))


;;; Version information

;;;###autoload
(defun projectile-version (&optional show-version)
  "Get the Projectile version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (if (require 'pkg-info nil t)
      (let ((version (pkg-info-version-info 'projectile)))
        (when show-version
          (message "Projectile %s" version))
        version)
    (error "Cannot determine version without package pkg-info")))


;;; Caching
(defvar projectile-file-exists-cache
  (make-hash-table :test 'equal)
  "Cached `projectile-file-exists-p' results.")

(defvar projectile-file-exists-cache-timer nil
  "Timer for scheduling`projectile-file-exists-cache-cleanup'.")

(defun projectile-file-exists-cache-cleanup ()
  "Removed timed out cache entries and reschedules or remove the
timer if no more items are in the cache."
  (let ((now (current-time)))
    (maphash (lambda (key value)
               (if (time-less-p (cdr value) now)
                   (remhash key projectile-file-exists-cache)))
             projectile-file-exists-cache)
    (setq projectile-file-exists-cache-timer
          (if (> (hash-table-count projectile-file-exists-cache) 0)
              (run-with-timer 10 nil 'projectile-file-exists-cache-cleanup)))))

(defun projectile-file-exists-p (filename)
  "Return t if file FILENAME exist.
A wrapper around `file-exists-p' with additional caching support."
  (let* ((file-remote (file-remote-p filename))
         (expire-seconds
          (if file-remote
              (and projectile-file-exists-remote-cache-expire
                   (> projectile-file-exists-remote-cache-expire 0)
                   projectile-file-exists-remote-cache-expire)
            (and projectile-file-exists-local-cache-expire
                 (> projectile-file-exists-local-cache-expire 0)
                 projectile-file-exists-local-cache-expire)))
         (remote-file-name-inhibit-cache (if expire-seconds
                                             expire-seconds
                                           remote-file-name-inhibit-cache)))
    (if (not expire-seconds)
        (file-exists-p filename)
      (let* ((current-time (current-time))
             (cached (gethash filename projectile-file-exists-cache))
             (cached-value (if cached (car cached)))
             (cached-expire (if cached (cdr cached)))
             (cached-expired (if cached (time-less-p cached-expire current-time) t))
             (value (or (and (not cached-expired) cached-value)
                        (if (file-exists-p filename) 'found 'notfound))))
        (when (or (not cached) cached-expired)
          (puthash filename
                   (cons value (time-add current-time (seconds-to-time expire-seconds)))
                   projectile-file-exists-cache))
        (unless projectile-file-exists-cache-timer
          (setq projectile-file-exists-cache-timer
                (run-with-timer 10 nil 'projectile-file-exists-cache-cleanup)))
        (equal value 'found)))))

;;;###autoload
(defun projectile-invalidate-cache (arg)
  "Remove the current project's files from `projectile-projects-cache'.

With a prefix argument ARG prompts for the name of the project whose cache
to invalidate."
  (interactive "P")
  (let ((project-root
         (if arg
             (completing-read "Remove cache for: "
                              (projectile-hash-keys projectile-projects-cache))
           (projectile-project-root))))
    (setq projectile-project-root-cache (make-hash-table :test 'equal))
    (remhash project-root projectile-project-type-cache)
    (remhash project-root projectile-projects-cache)
    (projectile-serialize-cache)
    (when projectile-verbose
      (message "Invalidated Projectile cache for %s."
               (propertize project-root 'face 'font-lock-keyword-face))))
  (when (fboundp 'recentf-cleanup)
    (recentf-cleanup)))

(defun projectile-cache-project (project files)
  "Cache PROJECTs FILES.
The cache is created both in memory and on the hard drive."
  (when projectile-enable-caching
    (puthash project files projectile-projects-cache)
    (projectile-serialize-cache)))

;;;###autoload
(defun projectile-purge-file-from-cache (file)
  "Purge FILE from the cache of the current project."
  (interactive
   (list (projectile-completing-read
          "Remove file from cache: "
          (projectile-current-project-files))))
  (let* ((project-root (projectile-project-root))
         (project-cache (gethash project-root projectile-projects-cache)))
    (if (projectile-file-cached-p file project-root)
        (progn
          (puthash project-root (remove file project-cache) projectile-projects-cache)
          (projectile-serialize-cache)
          (when projectile-verbose
            (message "%s removed from cache" file)))
      (error "%s is not in the cache" file))))

;;;###autoload
(defun projectile-purge-dir-from-cache (dir)
  "Purge DIR from the cache of the current project."
  (interactive
   (list (projectile-completing-read
          "Remove directory from cache: "
          (projectile-current-project-dirs))))
  (let* ((project-root (projectile-project-root))
         (project-cache (gethash project-root projectile-projects-cache)))
    (puthash project-root
             (cl-remove-if (lambda (str) (string-prefix-p dir str)) project-cache)
             projectile-projects-cache)))

(defun projectile-file-cached-p (file project)
  "Check if FILE is already in PROJECT cache."
  (member file (gethash project projectile-projects-cache)))

;;;###autoload
(defun projectile-cache-current-file ()
  "Add the currently visited file to the cache."
  (interactive)
  (let ((current-project (projectile-project-root)))
    (when (and (buffer-file-name) (gethash (projectile-project-root) projectile-projects-cache))
      (let* ((abs-current-file (file-truename (buffer-file-name)))
             (current-file (file-relative-name abs-current-file current-project)))
        (unless (or (projectile-file-cached-p current-file current-project)
                    (projectile-ignored-directory-p (file-name-directory abs-current-file))
                    (projectile-ignored-file-p abs-current-file))
          (puthash current-project
                   (cons current-file (gethash current-project projectile-projects-cache))
                   projectile-projects-cache)
          (projectile-serialize-cache)
          (message "File %s added to project %s cache."
                   (propertize current-file 'face 'font-lock-keyword-face)
                   (propertize current-project 'face 'font-lock-keyword-face)))))))

;; cache opened files automatically to reduce the need for cache invalidation
(defun projectile-cache-files-find-file-hook ()
  "Function for caching files with `find-file-hook'."
  (let ((project-root (projectile-project-p)))
    (when (and projectile-enable-caching
               project-root
               (not (projectile-ignored-project-p project-root)))
      (projectile-cache-current-file))))

(defun projectile-track-known-projects-find-file-hook ()
  "Function for caching projects with `find-file-hook'."
  (when (and projectile-track-known-projects-automatically (projectile-project-p))
    (let ((known-projects (and (sequencep projectile-known-projects)
                               (copy-sequence projectile-known-projects))))
      (projectile-add-known-project (projectile-project-root))
      (unless (equal known-projects projectile-known-projects)
        (projectile-merge-known-projects)))))


(defun projectile-maybe-invalidate-cache (force)
  "Invalidate if FORCE or project's dirconfig newer than cache."
  (when (or force (file-newer-than-file-p (projectile-dirconfig-file)
                                          projectile-cache-file))
    (projectile-invalidate-cache nil)))

;;;###autoload
(defun projectile-discover-projects-in-directory (directory)
  "Discover any projects in DIRECTORY and add them to the projectile cache.
This function is not recursive and only adds projects with roots
at the top level of DIRECTORY."
  (interactive
   (list (read-directory-name "Starting directory: ")))
  (let ((subdirs (directory-files directory t)))
    (mapcar
     (lambda (dir)
       (when (and (file-directory-p dir)
                  (not (member (file-name-nondirectory dir) '(".." "."))))
         (let ((default-directory dir))
           (when (projectile-project-p)
             (projectile-add-known-project (projectile-project-root))))))
     subdirs)))


(defadvice delete-file (before purge-from-projectile-cache (filename &optional trash))
  (if (and projectile-enable-caching (projectile-project-p))
      (let* ((project-root (projectile-project-root))
             (true-filename (file-truename filename))
             (relative-filename (file-relative-name true-filename project-root)))
        (if (projectile-file-cached-p relative-filename project-root)
            (projectile-purge-file-from-cache relative-filename)))))


;;; Project root related utilities
(defun projectile-parent (path)
  "Return the parent directory of PATH.
PATH may be a file or directory and directory paths may end with a slash."
  (directory-file-name (file-name-directory (directory-file-name (expand-file-name path)))))

(defun projectile-locate-dominating-file (file name)
  "Look up the directory hierarchy from FILE for a directory containing NAME.
Stop at the first parent directory containing a file NAME,
and return the directory.  Return nil if not found.
Instead of a string, NAME can also be a predicate taking one argument
\(a directory) and returning a non-nil value if that directory is the one for
which we're looking."
  ;; copied from files.el (stripped comments) emacs-24 bzr branch 2014-03-28 10:20
  (setq file (abbreviate-file-name file))
  (let ((root nil)
        try)
    (while (not (or root
                    (null file)
                    (string-match locate-dominating-stop-dir-regexp file)))
      (setq try (if (stringp name)
                    (projectile-file-exists-p (expand-file-name name file))
                  (funcall name file)))
      (cond (try (setq root file))
            ((equal file (setq file (file-name-directory
                                     (directory-file-name file))))
             (setq file nil))))
    (and root (expand-file-name (file-name-as-directory root)))))

(defvar-local projectile-project-root nil
  "Defines a custom Projectile project root.
This is intended to be used as a file local variable.")

(defun projectile-root-local (_dir)
  "A simple wrapper around `projectile-project-root'."
  projectile-project-root)

(defun projectile-root-top-down (dir &optional list)
  "Identify a project root in DIR by top-down search for files in LIST.
If LIST is nil, use `projectile-project-root-files' instead.
Return the first (topmost) matched directory or nil if not found."
  (projectile-locate-dominating-file
   dir
   (lambda (dir)
     (cl-find-if (lambda (f) (projectile-file-exists-p (expand-file-name f dir)))
                 (or list projectile-project-root-files)))))

(defun projectile-root-bottom-up (dir &optional list)
  "Identify a project root in DIR by bottom-up search for files in LIST.
If LIST is nil, use `projectile-project-root-files-bottom-up' instead.
Return the first (bottommost) matched directory or nil if not found."
  (cl-some (lambda (name) (projectile-locate-dominating-file dir name))
           (or list projectile-project-root-files-bottom-up)))

(defun projectile-root-top-down-recurring (dir &optional list)
  "Identify a project root in DIR by recurring top-down search for files in LIST.
If LIST is nil, use `projectile-project-root-files-top-down-recurring'
instead.  Return the last (bottommost) matched directory in the
topmost sequence of matched directories.  Nil otherwise."
  (cl-some
   (lambda (f)
     (projectile-locate-dominating-file
      dir
      (lambda (dir)
        (and (projectile-file-exists-p (expand-file-name f dir))
             (or (string-match locate-dominating-stop-dir-regexp (projectile-parent dir))
                 (not (projectile-file-exists-p (expand-file-name f (projectile-parent dir)))))))))
   (or list projectile-project-root-files-top-down-recurring)))

(defvar-local projectile-cached-project-root nil
  "Cached root of the current Projectile project. If non-nil, it
is used as the return value of `projectile-project-root' for
performance (unless the variable `projectile-project-root' is
also set). If nil, it is recalculated the next time
`projectile-project-root' is called.

This variable is reset automatically when Projectile detects that
the `buffer-file-name' has changed. It can also be reset manually
by calling `projectile-reset-cached-project-root'.")

(defvar-local projectile-cached-buffer-file-name nil
  "The last known value of `buffer-file-name' for the current
buffer. This is used to detect a change in `buffer-file-name',
which triggers a reset of `projectile-cached-project-root' and
`projectile-cached-project-name'.")

(defun projectile-project-root ()
  "Retrieves the root directory of a project if available.
The current directory is assumed to be the project's root otherwise.

When not in project the behaviour of the function is controlled by
`projectile-require-project-root'.  If it's set to nil the function
will return the current directory, otherwise it'd raise an error."
  ;; the cached value will be 'none in the case of no project root (this is to
  ;; ensure it is not reevaluated each time when not inside a project) so use
  ;; cl-subst to replace this 'none value with nil so a nil value is used
  ;; instead
  (or (cl-subst nil 'none
                (or (and (equal projectile-cached-buffer-file-name buffer-file-name)
                         projectile-cached-project-root)
                    (progn
                      (setq projectile-cached-buffer-file-name buffer-file-name)
                      (setq projectile-cached-project-root
                            ;; The `is-local' and `is-connected' variables are
                            ;; used to fix the behavior where Emacs hangs
                            ;; because of Projectile when you open a file over
                            ;; TRAMP. It basically prevents Projectile from
                            ;; trying to find information about files for which
                            ;; it's not possible to get that information right
                            ;; now.
                            (or (let* ((dir default-directory)
                                       (is-local (not (file-remote-p dir)))      ;; `true' if the file is local
                                       (is-connected (file-remote-p dir nil t))) ;; `true' if the file is remote AND we are connected to the remote
                                  (when (or is-local is-connected)
                                    (cl-some
                                     (lambda (func)
                                       (let* ((cache-key (format "%s-%s" func dir))
                                              (cache-value (gethash cache-key projectile-project-root-cache)))
                                         (if (and cache-value (file-exists-p cache-value))
                                             cache-value
                                           (let ((value (funcall func (file-truename dir))))
                                             (puthash cache-key value projectile-project-root-cache)
                                             value))))
                                     projectile-project-root-files-functions)))
                                ;; set cached to none so is non-nil so we don't try
                                ;; and look it up again
                                'none)))))
      (if projectile-require-project-root
          (error "You're not in a project")
        default-directory)))

(defun projectile-file-truename (file-name)
  "Return the truename of FILE-NAME.
A thin wrapper around `file-truename' that handles nil."
  (when file-name
    (file-truename file-name)))

(defun projectile-project-p ()
  "Check if we're in a project."
  (condition-case nil
      (projectile-project-root)
    (error nil)))

(defun projectile-default-project-name (project-root)
  "Default function used create project name to be displayed based on the value of PROJECT-ROOT."
  (file-name-nondirectory (directory-file-name project-root)))

(defvar-local projectile-cached-project-name nil
  "Cached name of the current Projectile project. If non-nil, it
is used as the return value of `projectile-project-name' for
performance (unless the variable `projectile-project-name' is
also set). If nil, it is recalculated the next time
`projectile-project-name' is called.

This variable is reset automatically when Projectile detects that
the `buffer-file-name' has changed. It can also be reset manually
by calling `projectile-reset-cached-project-name'.")

(defun projectile-reset-cached-project-root ()
  "Reset the value of `projectile-cached-project-root' to nil.

This means that it is automatically recalculated the next time
function `projectile-project-root' is called."
  (interactive)
  (setq projectile-cached-project-root nil))

(defun projectile-reset-cached-project-name ()
  "Reset the value of `projectile-cached-project-name' to nil.

This means that it is automatically recalculated the next time
function `projectile-project-name' is called."
  (interactive)
  (setq projectile-cached-project-name nil))

(defun projectile-project-name ()
  "Return project name."
  (or projectile-project-name
      (and (equal projectile-cached-buffer-file-name buffer-file-name)
           projectile-cached-project-name)
      (progn
        (setq projectile-cached-buffer-file-name buffer-file-name)
        (setq projectile-cached-project-name
              (let ((project-root
                     (condition-case nil
                         (projectile-project-root)
                       (error nil))))
                (if project-root
                    (funcall projectile-project-name-function project-root)
                  "-"))))))


;;; Project indexing
(defun projectile-get-project-directories ()
  "Get the list of project directories that are of interest to the user."
  (mapcar (lambda (subdir) (concat (projectile-project-root) subdir))
          (or (nth 0 (projectile-parse-dirconfig-file)) '(""))))

(defun projectile-dir-files (directory)
  "List the files in DIRECTORY and in its sub-directories.
Files are returned as relative paths to the project root."
  ;; check for a cache hit first if caching is enabled
  (let ((files-list (and projectile-enable-caching
                         (gethash directory projectile-projects-cache)))
        (root (projectile-project-root)))
    ;; cache disabled or cache miss
    (or files-list
        (if (eq projectile-indexing-method 'native)
            (projectile-dir-files-native root directory)
          ;; use external tools to get the project files
          (projectile-adjust-files (projectile-dir-files-external root directory))))))

(defun projectile-dir-files-native (root directory)
  "Get the files for ROOT under DIRECTORY using just Emacs Lisp."
  (let ((progress-reporter
         (make-progress-reporter
          (format "Projectile is indexing %s"
                  (propertize directory 'face 'font-lock-keyword-face)))))
    ;; we need the files with paths relative to the project root
    (mapcar (lambda (file) (file-relative-name file root))
            (projectile-index-directory directory (projectile-filtering-patterns)
                                        progress-reporter))))

(defun projectile-dir-files-external (root directory)
  "Get the files for ROOT under DIRECTORY using external tools."
  (let ((default-directory directory))
    (mapcar (lambda (file)
              (file-relative-name (expand-file-name file directory) root))
            (projectile-get-repo-files))))

(defcustom projectile-git-command "git ls-files -zco --exclude-standard"
  "Command used by projectile to get the files in a git project."
  :group 'projectile
  :type 'string)

(defcustom projectile-git-submodule-command "git submodule --quiet foreach 'echo $path' | tr '\\n' '\\0'"
  "Command used by projectile to get the files in git submodules."
  :group 'projectile
  :type 'string)

(defcustom projectile-git-ignored-command "git ls-files -zcoi --exclude-standard"
  "Command used by projectile to get the ignored files in a git project."
  :group 'projectile
  :type 'string
  :package-version '(projectile . "0.14.0"))

(defcustom projectile-hg-command "hg locate -f -0 -I ."
  "Command used by projectile to get the files in a hg project."
  :group 'projectile
  :type 'string)

(defcustom projectile-fossil-command (concat "fossil ls | "
                                             (when (string-equal system-type
                                                                 "windows-nt")
                                               "dos2unix | ")
                                             "tr '\\n' '\\0'")
  "Command used by projectile to get the files in a fossil project."
  :group 'projectile
  :type 'string)

(defcustom projectile-bzr-command "bzr ls -R --versioned -0"
  "Command used by projectile to get the files in a bazaar project."
  :group 'projectile
  :type 'string)

(defcustom projectile-darcs-command "darcs show files -0 . "
  "Command used by projectile to get the files in a darcs project."
  :group 'projectile
  :type 'string)

(defcustom projectile-svn-command "svn list -R . | grep -v '$/' | tr '\\n' '\\0'"
  "Command used by projectile to get the files in a svn project."
  :group 'projectile
  :type 'string)

(defcustom projectile-generic-command "find . -type f -print0"
  "Command used by projectile to get the files in a generic project."
  :group 'projectile
  :type 'string)

(defcustom projectile-vcs-dirty-state '("edited" "unregistered" "needs-update" "needs-merge" "unlocked-changes" "conflict")
  "List of states checked by `projectile-browse-dirty-projects'.
Possible checked states are:
\"edited\", \"unregistered\", \"needs-update\", \"needs-merge\", unlocked-changes\" and \"conflict\",
as defined in `vc.el'."
  :group 'projectile
  :type '(repeat (string))
  :package-version '(projectile . "0.15.0"))

(defun projectile-get-ext-command ()
  "Determine which external command to invoke based on the project's VCS."
  (let ((vcs (projectile-project-vcs)))
    (cond
     ((eq vcs 'git) projectile-git-command)
     ((eq vcs 'hg) projectile-hg-command)
     ((eq vcs 'fossil) projectile-fossil-command)
     ((eq vcs 'bzr) projectile-bzr-command)
     ((eq vcs 'darcs) projectile-darcs-command)
     ((eq vcs 'svn) projectile-svn-command)
     (t projectile-generic-command))))

(defun projectile-get-sub-projects-command ()
  (let ((vcs (projectile-project-vcs)))
    (cond
     ((eq vcs 'git) projectile-git-submodule-command)
     (t ""))))

(defun projectile-get-ext-ignored-command ()
  "Determine which external command to invoke based on the project's VCS."
  (let ((vcs (projectile-project-vcs)))
    (cond
     ((eq vcs 'git) projectile-git-ignored-command)
     ;; TODO: Add support for other VCS
     (t nil))))

(defun projectile-flatten (lst)
  "Take a nested list LST and return its contents as a single, flat list."
  (if (and (listp lst) (listp (cdr lst)))
      (cl-mapcan 'projectile-flatten lst)
    (list lst)))

(defun projectile-get-all-sub-projects (project)
  "Get all sub-projects for a given project.

PROJECT is base directory to start search recursively."
  (let ((submodules (projectile-get-immediate-sub-projects project)))
    (cond
     ((null submodules)
      nil)
     (t
      (nconc submodules (projectile-flatten
                         ;; recursively get sub-projects of each sub-project
                         (mapcar (lambda (s)
                                   (projectile-get-all-sub-projects s)) submodules)))))))

(defun projectile-get-immediate-sub-projects (path)
  "Get immediate sub-projects for a given project without recursing.

PATH is the vcs root or project root from which to start
searching, and should end with an appropriate path delimiter, such as
'/' or a '\\'.

If the vcs get-sub-projects query returns results outside of path,
they are excluded from the results of this function."
  (let* ((default-directory path)
         ;; search for sub-projects under current project `project'
         (submodules (mapcar
                      (lambda (s)
                        (file-name-as-directory (expand-file-name s default-directory)))
                      (projectile-files-via-ext-command (projectile-get-sub-projects-command))))
         (project-child-folder-regex
          (concat "\\`"
                  (regexp-quote path))))

    ;; If project root is inside of an VCS folder, but not actually an
    ;; VCS root itself, submodules external to the project will be
    ;; included in the VCS get sub-projects result. Let's remove them.
    (cl-remove-if-not
     (lambda (submodule)
       (string-match-p project-child-folder-regex
                       submodule))
     submodules)))

(defun projectile-get-sub-projects-files ()
  "Get files from sub-projects recursively."
  (projectile-flatten
   (mapcar (lambda (s)
             (let ((default-directory s))
               (mapcar (lambda (f)
                         (concat s f))
                       (projectile-files-via-ext-command projectile-git-command))))
           (condition-case nil
               (projectile-get-all-sub-projects (projectile-project-root))
             (error nil)))))

(defun projectile-get-repo-files ()
  "Get a list of the files in the project, including sub-projects."
  (cond
   ((eq (projectile-project-vcs) 'git)
    (nconc (projectile-files-via-ext-command (projectile-get-ext-command))
           (projectile-get-sub-projects-files)))
   (t (projectile-files-via-ext-command (projectile-get-ext-command)))))

(defun projectile-get-repo-ignored-files ()
  "Get a list of the files ignored in the project."
  (let ((cmd (projectile-get-ext-ignored-command)))
    (when cmd
      (projectile-files-via-ext-command cmd))))

(defun projectile-get-repo-ignored-directory (dir)
  "Get a list of the files ignored in the project in the directory DIR."
  (let ((cmd (projectile-get-ext-ignored-command)))
    (when cmd
      (projectile-files-via-ext-command (concat cmd " " dir)))))

(defun projectile-call-process-to-string (program &rest args)
  "Invoke the executable PROGRAM with ARGS and return the output as a string."
  (with-temp-buffer
    (apply 'call-process program nil (current-buffer) nil args)
    (buffer-string)))

(defun projectile-shell-command-to-string (command)
  "Try to run COMMAND without actually using a shell and return the output.

The function `eshell-search-path' will be used to search the PATH
environment variable for an appropriate executable using the text
occuring before the first space.  If no executable is found,
fallback to `shell-command-to-string'."
  (cl-destructuring-bind
      (the-command . args) (split-string command " ")
    (let ((binary-path (eshell-search-path the-command)))
      (if binary-path
          (apply 'projectile-call-process-to-string binary-path args)
        (shell-command-to-string command)))))

(defun projectile-check-vcs-status (&optional PROJECT-PATH)
  "Check the status of the current project.
If PROJECT-PATH is a project, check this one instead."
  (let* ((PROJECT-PATH (or PROJECT-PATH (projectile-project-root)))
         (project-status nil))
    (save-excursion
      (vc-dir PROJECT-PATH)
      ;; wait until vc-dir is done
      (while (vc-dir-busy) (sleep-for 0 100))
      ;; check for status
      (save-excursion
        (save-match-data
          (dolist (check projectile-vcs-dirty-state)
            (goto-char (point-min))
            (when (search-forward check nil t)
              (setq project-status (cons check project-status))))))
      (kill-buffer)
      project-status)))

(defun projectile-check-vcs-status-of-known-projects ()
  "Return the list of dirty projects.
The list is composed of sublists~: (project-path, project-status).
Raise an error if their is no dirty project."
  (let ((projects projectile-known-projects)
        (status ()))
    (dolist (project projects)
      (when (and (projectile-keep-project-p project) (not (string= 'none (projectile-project-vcs project))))
        (let ((tmp-status (projectile-check-vcs-status project)))
          (when tmp-status
            (setq status (cons (list project tmp-status) status))))))
    (when (= (length status) 0)
      (message "No dirty projects have been found"))
    status))

(defun projectile-browse-dirty-projects ()
  "Browse dirty version controlled projects."
  (interactive)
  (let ((status nil)
        (mod-proj nil))
    (message "Checking for modifications in known projects...")
    (setq status (projectile-check-vcs-status-of-known-projects))
    (while (not (= (length status) 0))
      (setq mod-proj (cons (car (pop status)) mod-proj)))
    (projectile-vc
     (projectile-completing-read "Select project: " mod-proj))))

(defun projectile-files-via-ext-command (command)
  "Get a list of relative file names in the project root by executing COMMAND."
  (split-string (shell-command-to-string command) "\0" t))

(defun projectile-index-directory (directory patterns progress-reporter)
  "Index DIRECTORY taking into account PATTERNS.
The function calls itself recursively until all sub-directories
have been indexed.  The PROGRESS-REPORTER is updated while the
function is executing."
  (apply 'append
         (mapcar
          (lambda (f)
            (unless (or (and patterns (projectile-ignored-rel-p f directory patterns))
                        (member (file-name-nondirectory (directory-file-name f))
                                '("." ".." ".svn" ".cvs")))
              (progress-reporter-update progress-reporter)
              (if (file-directory-p f)
                  (unless (projectile-ignored-directory-p
                           (file-name-as-directory f))
                    (projectile-index-directory f patterns progress-reporter))
                (unless (projectile-ignored-file-p f)
                  (list f)))))
          (directory-files directory t))))

(defun projectile-adjust-files (files)
  "First remove ignored files from FILES, then add back unignored files."
  (projectile-add-unignored (projectile-remove-ignored files)))

(defun projectile--stringi= (string1 string2)
  "Match STRING1 and STRING2 case insensitively."
  (equal (compare-strings string1 nil nil string2 nil nil t) t))

(defun projectile-remove-ignored (files)
  "Remove ignored files and folders from FILES.

If ignored directory prefixed with '*', then ignore all
directories/subdirectories with matching filename,
otherwise operates relative to project root."
  (let ((ignored-files (projectile-ignored-files-rel))
        (ignored-dirs (projectile-ignored-directories-rel)))
    (cl-remove-if
     (lambda (file)
       (or (cl-some
            (lambda (f)
              (string= f (file-name-nondirectory file)))
            ignored-files)
           (cl-some
            (lambda (dir)
              ;; if the directory is prefixed with '*' then ignore all directories matching that name
              (if (string-prefix-p "*" dir)
                  ;; remove '*' and trailing slash from ignored directory name
                  (let ((d (substring dir 1 (if (equal (substring dir -1) "/") -1 nil))))
                    (cl-some
                     (lambda (p)
                       (string= d p))
                     ;; split path by '/', remove empty strings, and check if any subdirs match name 'd'
                     (delete "" (split-string (or (file-name-directory file) "") "/"))))
                (string-prefix-p dir file)))
            ignored-dirs)
           (cl-some
            (lambda (suf)
              (projectile--stringi= suf (file-name-extension file t)))
            projectile-globally-ignored-file-suffixes)))
     files)))

(defun projectile-keep-ignored-files (files)
  "Filter FILES to retain only those that are ignored."
  (when files
    (cl-remove-if-not
     (lambda (file)
       (cl-some (lambda (f) (string-prefix-p f file)) files))
     (projectile-get-repo-ignored-files))))

(defun projectile-keep-ignored-directories (directories)
  "Get ignored files within each of DIRECTORIES."
  (when directories
    (let (result)
      (dolist (dir directories result)
        (setq result (append result
                             (projectile-get-repo-ignored-directory dir))))
      result)))

(defun projectile-add-unignored (files)
  "This adds unignored files to FILES.

Useful because the VCS may not return ignored files at all.  In
this case unignored files will be absent from FILES."
  (let ((unignored-files (projectile-keep-ignored-files
                          (projectile-unignored-files-rel)))
        (unignored-paths (projectile-remove-ignored
                          (projectile-keep-ignored-directories
                           (projectile-unignored-directories-rel)))))
    (append files unignored-files unignored-paths)))

(defun projectile-buffers-with-file (buffers)
  "Return only those BUFFERS backed by files."
  (cl-remove-if-not (lambda (b) (buffer-file-name b)) buffers))

(defun projectile-buffers-with-file-or-process (buffers)
  "Return only those BUFFERS backed by files or processes."
  (cl-remove-if-not (lambda (b) (or (buffer-file-name b)
                                    (get-buffer-process b))) buffers))

(defun projectile-project-buffers ()
  "Get a list of project buffers."
  (let* ((project-root (projectile-project-root))
         (all-buffers (cl-remove-if-not
                       (lambda (buffer)
                         (projectile-project-buffer-p buffer project-root))
                       (buffer-list))))
    (if projectile-buffers-filter-function
        (funcall projectile-buffers-filter-function all-buffers)
      all-buffers)))

(defun projectile-process-current-project-buffers (action)
  "Process the current project's buffers using ACTION."
  (let ((project-buffers (projectile-project-buffers)))
    (dolist (buffer project-buffers)
      (funcall action buffer))))

(defun projectile-project-buffer-files ()
  "Get a list of project buffer files."
  (let ((project-root (projectile-project-root)))
    (mapcar
     (lambda (buffer)
       (file-relative-name
        (buffer-file-name buffer)
        project-root))
     (projectile-buffers-with-file
      (projectile-project-buffers)))))

(defun projectile-project-buffer-p (buffer project-root)
  "Check if BUFFER is under PROJECT-ROOT."
  (with-current-buffer buffer
    (and (not (string-prefix-p " " (buffer-name buffer)))
         (not (projectile-ignored-buffer-p buffer))
         (string-equal (file-remote-p default-directory)
                       (file-remote-p project-root))
         (not (string-match-p "^http\\(s\\)?://" default-directory))
         (string-prefix-p project-root (file-truename default-directory) (eq system-type 'windows-nt)))))

(defun projectile-ignored-buffer-p (buffer)
  "Check if BUFFER should be ignored."
  (or
   (member (buffer-name buffer) projectile-globally-ignored-buffers)
   (with-current-buffer buffer
     (cl-some
      (lambda (mode)
        (string-match-p (concat "^" mode "$")
                        (symbol-name major-mode)))
      projectile-globally-ignored-modes))))

(defun projectile-difference (list1 list2)
  (cl-remove-if
   (lambda (x) (member x list2))
   list1))

(defun projectile-recently-active-files ()
  "Get list of recently active files.

Files are ordered by recently active buffers, and then recently
opened through use of recentf."
  (let ((project-buffer-files (projectile-project-buffer-files)))
    (append project-buffer-files
            (projectile-difference
             (projectile-recentf-files)
             project-buffer-files))))

(defun projectile-project-buffer-names ()
  "Get a list of project buffer names."
  (mapcar #'buffer-name (projectile-project-buffers)))

(defun projectile-prepend-project-name (string)
  "Prepend the current project's name to STRING."
  (format "[%s] %s" (projectile-project-name) string))

(defun projectile-read-buffer-to-switch (prompt)
  "Read the name of a buffer to switch to, prompting with PROMPT.

This function excludes the current buffer from the offered
choices."
  (projectile-completing-read
   prompt
   (delete (buffer-name (current-buffer))
           (projectile-project-buffer-names))))

;;;###autoload
(defun projectile-switch-to-buffer ()
  "Switch to a project buffer."
  (interactive)
  (switch-to-buffer
   (projectile-read-buffer-to-switch "Switch to buffer: ")))

;;;###autoload
(defun projectile-switch-to-buffer-other-window ()
  "Switch to a project buffer and show it in another window."
  (interactive)
  (switch-to-buffer-other-window
   (projectile-read-buffer-to-switch "Switch to buffer: ")))

;;;###autoload
(defun projectile-switch-to-buffer-other-frame ()
  "Switch to a project buffer and show it in another window."
  (interactive)
  (switch-to-buffer-other-frame
   (projectile-read-buffer-to-switch "Switch to buffer: ")))

;;;###autoload
(defun projectile-display-buffer ()
  "Display a project buffer in another window without selecting it."
  (interactive)
  (display-buffer
   (projectile-completing-read
    "Display buffer: "
    (projectile-project-buffer-names))))

;;;###autoload
(defun projectile-project-buffers-other-buffer ()
  "Switch to the most recently selected buffer project buffer.
Only buffers not visible in windows are returned."
  (interactive)
  (switch-to-buffer (car (projectile-project-buffers-non-visible))) nil t)

(defun projectile-project-buffers-non-visible ()
  "Get a list of non visible project buffers."
  (cl-remove-if-not
   (lambda (buffer)
     (not (get-buffer-window buffer 'visible)))
   (projectile-project-buffers)))

;;;###autoload
(defun projectile-multi-occur (&optional nlines)
  "Do a `multi-occur' in the project's buffers.
With a prefix argument, show NLINES of context."
  (interactive "P")
  (multi-occur (projectile-project-buffers)
               (car (occur-read-primary-args))
               nlines))

(defun projectile-normalise-paths (patterns)
  "Remove leading `/' from the elements of PATTERNS."
  (delq nil (mapcar (lambda (pat) (and (string-prefix-p "/" pat)
                                       ;; remove the leading /
                                       (substring pat 1)))
                    patterns)))

(defun projectile-expand-paths (paths)
  "Expand the elements of PATHS.

Elements containing wildcards are expanded and spliced into the
resulting paths.  The returned PATHS are absolute, based on the
projectile project root."
  (let ((default-directory (projectile-project-root)))
    (projectile-flatten (mapcar
                         (lambda (pattern)
                           (or (file-expand-wildcards pattern t)
                               (projectile-expand-root pattern)))
                         paths))))

(defun projectile-normalise-patterns (patterns)
  "Remove paths from PATTERNS."
  (cl-remove-if (lambda (pat) (string-prefix-p "/" pat)) patterns))

(defun projectile-make-relative-to-root (files)
  "Make FILES relative to the project root."
  (let ((project-root (projectile-project-root)))
    (mapcar (lambda (f) (file-relative-name f project-root)) files)))

(defun projectile-ignored-directory-p (directory)
  "Check if DIRECTORY should be ignored."
  (member directory (projectile-ignored-directories)))

(defun projectile-ignored-file-p (file)
  "Check if FILE should be ignored."
  (member file (projectile-ignored-files)))

(defun projectile-check-pattern-p (file pattern)
  "Check if FILE meets PATTERN."
  (or (string-suffix-p (directory-file-name pattern)
                       (directory-file-name file))
      (member file (file-expand-wildcards pattern t))))

(defun projectile-ignored-rel-p (file directory patterns)
  "Check if FILE should be ignored relative to DIRECTORY
according to PATTERNS: (ignored . unignored)"
  (let ((default-directory directory))
    (and (cl-some
          (lambda (pat) (projectile-check-pattern-p file pat))
          (car patterns))
         (cl-notany
          (lambda (pat) (projectile-check-pattern-p file pat))
          (cdr patterns)))))

(defun projectile-ignored-files ()
  "Return list of ignored files."
  (projectile-difference
   (mapcar
    #'projectile-expand-root
    (append
     projectile-globally-ignored-files
     (projectile-project-ignored-files)))
   (projectile-unignored-files)))

(defun projectile-ignored-directories ()
  "Return list of ignored directories."
  (projectile-difference
   (mapcar
    #'file-name-as-directory
    (mapcar
     #'projectile-expand-root
     (append
      projectile-globally-ignored-directories
      (projectile-project-ignored-directories))))
   (projectile-unignored-directories)))

(defun projectile-ignored-directories-rel ()
  "Return list of ignored directories, relative to the root."
  (projectile-make-relative-to-root (projectile-ignored-directories)))

(defun projectile-ignored-files-rel ()
  "Return list of ignored files, relative to the root."
  (projectile-make-relative-to-root (projectile-ignored-files)))

(defun projectile-project-ignored-files ()
  "Return list of project ignored files.
Unignored files are not included."
  (cl-remove-if 'file-directory-p (projectile-project-ignored)))

(defun projectile-project-ignored-directories ()
  "Return list of project ignored directories.
Unignored directories are not included."
  (cl-remove-if-not 'file-directory-p (projectile-project-ignored)))

(defun projectile-paths-to-ignore ()
  "Return a list of ignored project paths."
  (projectile-normalise-paths (nth 1 (projectile-parse-dirconfig-file))))

(defun projectile-patterns-to-ignore ()
  "Return a list of relative file patterns."
  (projectile-normalise-patterns (nth 1 (projectile-parse-dirconfig-file))))

(defun projectile-project-ignored ()
  "Return list of project ignored files/directories.
Unignored files/directories are not included."
  (let ((paths (projectile-paths-to-ignore)))
    (projectile-expand-paths paths)))

(defun projectile-unignored-files ()
  "Return list of unignored files."
  (mapcar
   #'projectile-expand-root
   (append
    projectile-globally-unignored-files
    (projectile-project-unignored-files))))

(defun projectile-unignored-directories ()
  "Return list of unignored directories."
  (mapcar
   #'file-name-as-directory
   (mapcar
    #'projectile-expand-root
    (append
     projectile-globally-unignored-directories
     (projectile-project-unignored-directories)))))

(defun projectile-unignored-directories-rel ()
  "Return list of unignored directories, relative to the root."
  (projectile-make-relative-to-root (projectile-unignored-directories)))

(defun projectile-unignored-files-rel ()
  "Return list of unignored files, relative to the root."
  (projectile-make-relative-to-root (projectile-unignored-files)))

(defun projectile-project-unignored-files ()
  "Return list of project unignored files."
  (cl-remove-if 'file-directory-p (projectile-project-unignored)))

(defun projectile-project-unignored-directories ()
  "Return list of project unignored directories."
  (cl-remove-if-not 'file-directory-p (projectile-project-unignored)))

(defun projectile-paths-to-ensure ()
  "Return a list of unignored project paths."
  (projectile-normalise-paths (nth 2 (projectile-parse-dirconfig-file))))

(defun projectile-files-to-ensure ()
  (projectile-flatten (mapcar (lambda (pat) (file-expand-wildcards pat t))
                              (projectile-patterns-to-ensure))))

(defun projectile-patterns-to-ensure ()
  "Return a list of relative file patterns."
  (projectile-normalise-patterns (nth 2 (projectile-parse-dirconfig-file))))

(defun projectile-filtering-patterns ()
  (cons (projectile-patterns-to-ignore)
        (projectile-patterns-to-ensure)))

(defun projectile-project-unignored ()
  "Return list of project ignored files/directories."
  (delete-dups (append (projectile-expand-paths (projectile-paths-to-ensure))
                       (projectile-expand-paths (projectile-files-to-ensure)))))


(defun projectile-dirconfig-file ()
  "Return the absolute path to the project's dirconfig file."
  (expand-file-name ".projectile" (projectile-project-root)))

(defun projectile-parse-dirconfig-file ()
  "Parse project ignore file and return directories to ignore and keep.

The return value will be a list of three elements, the car being
the list of directories to keep, the cadr being the list of files
or directories to ignore, and the caddr being the list of files
or directories to ensure.

Strings starting with + will be added to the list of directories
to keep, and strings starting with - will be added to the list of
directories to ignore.  For backward compatibility, without a
prefix the string will be assumed to be an ignore string."
  (let (keep ignore ensure (dirconfig (projectile-dirconfig-file)))
    (when (projectile-file-exists-p dirconfig)
      (with-temp-buffer
        (insert-file-contents dirconfig)
        (while (not (eobp))
          (pcase (char-after)
            (?+ (push (buffer-substring (1+ (point)) (line-end-position)) keep))
            (?- (push (buffer-substring (1+ (point)) (line-end-position)) ignore))
            (?! (push (buffer-substring (1+ (point)) (line-end-position)) ensure))
            (_ (push (buffer-substring (point) (line-end-position)) ignore)))
          (forward-line)))
      (list (mapcar (lambda (f) (file-name-as-directory (projectile-trim-string f)))
                    (delete "" (reverse keep)))
            (mapcar #'projectile-trim-string
                    (delete "" (reverse ignore)))
            (mapcar #'projectile-trim-string
                    (delete "" (reverse ensure)))))))

(defun projectile-expand-root (name)
  "Expand NAME to project root.

Never use on many files since it's going to recalculate the
project-root for every file."
  (expand-file-name name (projectile-project-root)))

(cl-defun projectile-completing-read (prompt choices &key initial-input action)
  "Present a project tailored PROMPT with CHOICES."
  (let ((prompt (projectile-prepend-project-name prompt))
        res)
    (setq res
          (cond
           ((eq projectile-completion-system 'ido)
            (ido-completing-read prompt choices nil nil initial-input))
           ((eq projectile-completion-system 'default)
            (completing-read prompt choices nil nil initial-input))
           ((eq projectile-completion-system 'helm)
            (if (and (fboundp 'helm)
                     (fboundp 'helm-make-source))
                (helm :sources
                      (helm-make-source "Projectile" 'helm-source-sync
                                        :candidates choices
                                        :action (if action
                                                    (prog1 action
                                                      (setq action nil))
                                                  #'identity))
                      :prompt prompt
                      :input initial-input
                      :buffer "*helm-projectile*")
              (user-error "Please install helm from \
https://github.com/emacs-helm/helm")))
           ((eq projectile-completion-system 'grizzl)
            (if (and (fboundp 'grizzl-completing-read)
                     (fboundp 'grizzl-make-index))
                (grizzl-completing-read prompt (grizzl-make-index choices))
              (user-error "Please install grizzl from \
https://github.com/d11wtq/grizzl")))
           ((eq projectile-completion-system 'ivy)
            (if (fboundp 'ivy-read)
                (ivy-read prompt choices
                          :initial-input initial-input
                          :action (prog1 action
                                    (setq action nil))
                          :caller 'projectile-completing-read)
              (user-error "Please install ivy from \
https://github.com/abo-abo/swiper")))
           (t (funcall projectile-completion-system prompt choices))))
    (if action
        (funcall action res)
      res)))

(defun projectile-current-project-files ()
  "Return a list of files for the current project."
  (let ((files (and projectile-enable-caching
                    (gethash (projectile-project-root) projectile-projects-cache))))
    ;; nothing is cached
    (unless files
      (when projectile-enable-caching
        (message "Empty cache. Projectile is initializing cache..."))
      (setq files (cl-mapcan
                   #'projectile-dir-files
                   (projectile-get-project-directories)))
      ;; cache the resulting list of files
      (when projectile-enable-caching
        (projectile-cache-project (projectile-project-root) files)))
    (projectile-sort-files files)))

(defun projectile-process-current-project-files (action)
  "Process the current project's files using ACTION."
  (let ((project-files (projectile-current-project-files))
        (default-directory (projectile-project-root)))
    (dolist (filename project-files)
      (funcall action filename))))

(defun projectile-current-project-dirs ()
  "Return a list of dirs for the current project."
  (delete-dups
   (delq nil
         (mapcar #'file-name-directory
                 (projectile-current-project-files)))))

(defun projectile-hash-keys (hash)
  "Return a list of all HASH keys."
  (let (allkeys)
    (maphash (lambda (k _v) (setq allkeys (cons k allkeys))) hash)
    allkeys))


;;; Interactive commands
(defcustom projectile-other-file-alist
  '( ;; handle C/C++ extensions
    ("cpp" . ("h" "hpp" "ipp"))
    ("ipp" . ("h" "hpp" "cpp"))
    ("hpp" . ("h" "ipp" "cpp" "cc"))
    ("cxx" . ("h" "hxx" "ixx"))
    ("ixx" . ("h" "hxx" "cxx"))
    ("hxx" . ("h" "ixx" "cxx"))
    ("c" . ("h"))
    ("m" . ("h"))
    ("mm" . ("h"))
    ("h" . ("c" "cc" "cpp" "ipp" "hpp" "cxx" "ixx" "hxx" "m" "mm"))
    ("cc" . ("h" "hh" "hpp"))
    ("hh" . ("cc"))

    ;; vertex shader and fragment shader extensions in glsl
    ("vert" . ("frag"))
    ("frag" . ("vert"))

    ;; handle files with no extension
    (nil . ("lock" "gpg"))
    ("lock" . (""))
    ("gpg" . (""))
    )
  "Alist of extensions for switching to file with the same name,
  using other extensions based on the extension of current
  file."
  :type 'alist)

(defun projectile--find-other-file (&optional flex-matching ff-variant)
  "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable
`projectile-other-file-alist'.  With FF-VARIANT set to a defun, use that
instead of `find-file'.   A typical example of such a defun would be
`find-file-other-window' or `find-file-other-frame'"
  (let ((ff (or ff-variant #'find-file))
        (other-files (projectile-get-other-files
                      (buffer-file-name)
                      (projectile-current-project-files)
                      flex-matching)))
    (if other-files
        (let ((file-name (if (= (length other-files) 1)
                             (car other-files)
                           (projectile-completing-read "Switch to: "
                                                       other-files))))
          (funcall ff (expand-file-name file-name
                                        (projectile-project-root))))
      (error "No other file found"))))

;;;###autoload
(defun projectile-find-other-file (&optional flex-matching)
  "Switch between files with the same name but different extensions.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching))

;;;###autoload
(defun projectile-find-other-file-other-window (&optional flex-matching)
  "Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching
                               #'find-file-other-window))

;;;###autoload
(defun projectile-find-other-file-other-frame (&optional flex-matching)
  "Switch between files with the same name but different extensions in other window.
With FLEX-MATCHING, match any file that contains the base name of current file.
Other file extensions can be customized with the variable `projectile-other-file-alist'."
  (interactive "P")
  (projectile--find-other-file flex-matching
                               #'find-file-other-frame))

(defun projectile--file-name-sans-extensions (file-name)
  "Return FILE-NAME sans any extensions.
The extensions, in a filename, are what follows the first '.', with the exception of a leading '.'"
  (setq file-name (file-name-nondirectory file-name))
  (substring file-name 0 (string-match "\\..*" file-name 1)))

(defun projectile--file-name-extensions (file-name)
  "Return FILE-NAME's extensions.
The extensions, in a filename, are what follows the first '.', with the exception of a leading '.'"
  ;;would it make sense to return nil instead of an empty string if no extensions are found?
  (setq file-name (file-name-nondirectory file-name))
  (let (extensions-start)
    (substring file-name
               (if (setq extensions-start (string-match "\\..*" file-name 1))
                   (1+ extensions-start)
                 (length file-name)))))

(defun projectile-associated-file-name-extensions (file-name)
  "Return projectile-other-file-extensions associated to FILE-NAME's extensions.
If no associated other-file-extensions for the complete (nested) extension are found, remove subextensions from FILENAME's extensions until a match is found."
  (let ((current-extensions (projectile--file-name-extensions (file-name-nondirectory file-name)))
        associated-extensions)
    (catch 'break
      (while (not (string= "" current-extensions))
        (if (setq associated-extensions (cdr (assoc current-extensions projectile-other-file-alist)))
            (throw 'break associated-extensions))
        (setq current-extensions (projectile--file-name-extensions current-extensions))))))

(defun projectile-get-other-files (current-file project-file-list &optional flex-matching)
  "Narrow to files with the same names but different extensions.
Returns a list of possible files for users to choose.

With FLEX-MATCHING, match any file that contains the base name of current file"
  (let* ((file-ext-list (projectile-associated-file-name-extensions current-file))
         (fulldirname (if (file-name-directory current-file)
                          (file-name-directory current-file) "./"))
         (dirname (file-name-nondirectory (directory-file-name fulldirname)))
         (filename (projectile--file-name-sans-extensions current-file))
         (file-list (mapcar (lambda (ext)
                              (if flex-matching
                                  (concat ".*" filename ".*" "\." ext "\\'")
                                (concat "^" filename
                                        (unless (equal ext "")
                                          (concat "\." ext))
                                        "\\'")))
                            file-ext-list))
         (candidates (cl-remove-if-not
                      (lambda (project-file)
                        (string-match filename project-file))
                      project-file-list))
         (candidates
          (projectile-flatten (mapcar
                               (lambda (file)
                                 (cl-remove-if-not
                                  (lambda (project-file)
                                    (string-match file
                                                  (concat (file-name-base project-file)
                                                          (unless (equal (file-name-extension project-file) nil)
                                                            (concat "\." (file-name-extension project-file))))))
                                  candidates))
                               file-list)))
         (candidates
          (cl-remove-if-not (lambda (file) (not (backup-file-name-p file))) candidates))
         (candidates
          (cl-sort (copy-sequence candidates)
                   (lambda (file _)
                     (let ((candidate-dirname (file-name-nondirectory (directory-file-name (file-name-directory file)))))
                       (unless (equal fulldirname (file-name-directory file))
                         (equal dirname candidate-dirname)))))))
    candidates))

(defun projectile-select-files (project-files &optional arg)
  "Select a list of files based on filename at point.

With a prefix ARG invalidates the cache first."
  (projectile-maybe-invalidate-cache arg)
  (let* ((file (if (region-active-p)
                   (buffer-substring (region-beginning) (region-end))
                 (or (thing-at-point 'filename) "")))
         (file (if (string-match "\\.?\\./" file)
                   (file-relative-name (file-truename file) (projectile-project-root))
                 file))
         (files (if file
                    (cl-remove-if-not
                     (lambda (project-file)
                       (string-match file project-file))
                     project-files)
                  nil)))
    files))

(defun projectile--find-file-dwim (invalidate-cache &optional ff-variant)
  "Jump to a project's files using completion based on context.

With a INVALIDATE-CACHE invalidates the cache first.

With FF-VARIANT set to a defun, use that instead of `find-file'.
A typical example of such a defun would be `find-file-other-window' or
`find-file-other-frame'

Subroutine for `projectile-find-file-dwim' and
`projectile-find-file-dwim-other-window'"
  (let* ((project-files (projectile-current-project-files))
         (files (projectile-select-files project-files invalidate-cache))
         (file (cond ((= (length files) 1)
                      (car files))
                     ((> (length files) 1)
                      (projectile-completing-read "Switch to: " files))
                     (t
                      (projectile-completing-read "Switch to: " project-files))))
         (ff (or ff-variant #'find-file)))
    (funcall ff (expand-file-name file (projectile-project-root)))
    (run-hooks 'projectile-find-file-hook)))

;;;###autoload
(defun projectile-find-file-dwim (&optional arg)
  "Jump to a project's files using completion based on context.

With a prefix ARG invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim' still switches to \"projectile/projectile.el\" immediately
 because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename like
 \"projectile/a\", a list of files with character 'a' in that directory is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim arg))

;;;###autoload
(defun projectile-find-file-dwim-other-window (&optional arg)
  "Jump to a project's files using completion based on context in other window.

With a prefix ARG invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-window' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-window' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim arg #'find-file-other-window))

;;;###autoload
(defun projectile-find-file-dwim-other-frame (&optional arg)
  "Jump to a project's files using completion based on context in other frame.

With a prefix ARG invalidates the cache first.

If point is on a filename, Projectile first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.  This works even
if the filename is incomplete, but there's only a single file in the current project
that matches the filename at point.  For example, if there's only a single file named
\"projectile/projectile.el\" but the current filename is \"projectile/proj\" (incomplete),
`projectile-find-file-dwim-other-frame' still switches to \"projectile/projectile.el\"
immediately because this is the only filename that matches.

- If it finds a list of files, the list is displayed for selecting.  A list of
files is displayed when a filename appears more than one in the project or the
filename at point is a prefix of more than two files in a project.  For example,
if `projectile-find-file-dwim-other-frame' is executed on a filepath like \"projectile/\", it lists
the content of that directory.  If it is executed on a partial filename
like \"projectile/a\", a list of files with character 'a' in that directory
is presented.

- If it finds nothing, display a list of all files in project for selecting."
  (interactive "P")
  (projectile--find-file-dwim arg #'find-file-other-frame))

(defun projectile--find-file (invalidate-cache &optional ff-variant)
  "Jump to a project's file using completion.
With INVALIDATE-CACHE invalidates the cache first.  With FF-VARIANT set to a
defun, use that instead of `find-file'.   A typical example of such a defun
would be `find-file-other-window' or `find-file-other-frame'"
  (interactive "P")
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let ((file (projectile-completing-read "Find file: "
                                          (projectile-current-project-files)))
        (ff (or ff-variant #'find-file)))
    (funcall ff (expand-file-name file (projectile-project-root)))
    (run-hooks 'projectile-find-file-hook)))

;;;###autoload
(defun projectile-find-file (&optional arg)
  "Jump to a project's file using completion.
With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile--find-file arg))

;;;###autoload
(defun projectile-find-file-other-window (&optional arg)
  "Jump to a project's file using completion and show it in another window.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile--find-file arg #'find-file-other-window))

;;;###autoload
(defun projectile-find-file-other-frame (&optional arg)
  "Jump to a project's file using completion and show it in another frame.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile--find-file arg #'find-file-other-frame))

(defun projectile-sort-files (files)
  "Sort FILES according to `projectile-sort-order'."
  (cl-case projectile-sort-order
    (default files)
    (recentf (projectile-sort-by-recentf-first files))
    (recently-active (projectile-sort-by-recently-active-first files))
    (modification-time (projectile-sort-by-modification-time files))
    (access-time (projectile-sort-by-access-time files))))

(defun projectile-sort-by-recentf-first (files)
  "Sort FILES by a recent first scheme."
  (let ((project-recentf-files (projectile-recentf-files)))
    (append project-recentf-files
            (projectile-difference files project-recentf-files))))

(defun projectile-sort-by-recently-active-first (files)
  "Sort FILES by most recently active buffers or opened files."
  (let ((project-recently-active-files (projectile-recently-active-files)))
    (append project-recently-active-files
            (projectile-difference files project-recently-active-files))))

(defun projectile-sort-by-modification-time (files)
  "Sort FILES by modification time."
  (let ((default-directory (projectile-project-root)))
    (cl-sort
     (copy-sequence files)
     (lambda (file1 file2)
       (let ((file1-mtime (nth 5 (file-attributes file1)))
             (file2-mtime (nth 5 (file-attributes file2))))
         (not (time-less-p file1-mtime file2-mtime)))))))

(defun projectile-sort-by-access-time (files)
  "Sort FILES by access time."
  (let ((default-directory (projectile-project-root)))
    (cl-sort
     (copy-sequence files)
     (lambda (file1 file2)
       (let ((file1-atime (nth 4 (file-attributes file1)))
             (file2-atime (nth 4 (file-attributes file2))))
         (not (time-less-p file1-atime file2-atime)))))))

(defun projectile--find-dir (invalidate-cache &optional dired-variant)
  "Jump to a project's directory using completion.

With INVALIDATE-CACHE invalidates the cache first.  With DIRED-VARIANT set to a
defun, use that instead of `dired'.  A typical example of such a defun would be
`dired-other-window' or `dired-other-frame'"
  (projectile-maybe-invalidate-cache invalidate-cache)
  (let ((dir (projectile-complete-dir))
        (dired-v (or dired-variant #'dired)))
    (funcall dired-v (expand-file-name dir (projectile-project-root)))
    (run-hooks 'projectile-find-dir-hook)))

;;;###autoload
(defun projectile-find-dir (&optional arg)
  "Jump to a project's directory using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile--find-dir arg))

;;;###autoload
(defun projectile-find-dir-other-window (&optional arg)
  "Jump to a project's directory in other window using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile--find-dir arg #'dired-other-window))

;;;###autoload
(defun projectile-find-dir-other-frame (&optional arg)
  "Jump to a project's directory in other window using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile--find-dir arg #'dired-other-frame))

(defun projectile-complete-dir ()
  (projectile-completing-read
   "Find dir: "
   (if projectile-find-dir-includes-top-level
       (append '("./") (projectile-current-project-dirs))
     (projectile-current-project-dirs))))

;;;###autoload
(defun projectile-find-test-file (&optional arg)
  "Jump to a project's test file using completion.

With a prefix ARG invalidates the cache first."
  (interactive "P")
  (projectile-maybe-invalidate-cache arg)
  (let ((file (projectile-completing-read "Find test file: "
                                          (projectile-current-project-test-files))))
    (find-file (expand-file-name file (projectile-project-root)))))

(defun projectile-test-files (files)
  "Return only the test FILES."
  (cl-remove-if-not 'projectile-test-file-p files))

(defun projectile-test-file-p (file)
  "Check if FILE is a test file."
  (or (cl-some (lambda (pat) (string-prefix-p pat (file-name-nondirectory file)))
               (delq nil (list (funcall projectile-test-prefix-function (projectile-project-type)))))
      (cl-some (lambda (pat) (string-suffix-p pat (file-name-sans-extension (file-name-nondirectory file))))
               (delq nil (list (funcall projectile-test-suffix-function (projectile-project-type)))))))

(defun projectile-current-project-test-files ()
  "Return a list of test files for the current project."
  (projectile-test-files (projectile-current-project-files)))

(defvar projectile-project-types (make-hash-table)
  "A hash table holding all project types that are known to Projectile.")

(cl-defun projectile-register-project-type
    (project-type marker-files &key compile test run test-suffix test-prefix)
  "Register a project type with projectile.

A project type is defined by PROJECT-TYPE, a set of MARKER-FILES,
and optional keyword arguments COMPILE which specifies a command
that builds the project, TEST which specified a command that
tests the project, RUN which specifies a command that runs the
project, TEST-SUFFIX which specifies test file suffix, and
TEST-PREFIX which specifies test file prefix."
  (let ((project-plist (list 'marker-files marker-files
                              'compile-command compile
                              'test-command test
                              'run-command run)))
    ;; There is no way for the function to distinguish between an
    ;; explicit argument of nil and an omitted argument. However, the
    ;; body of the function is free to consider nil an abbreviation
    ;; for some other meaningful value
    (when test-suffix
      (plist-put project-plist 'test-suffix test-suffix))
    (when test-prefix
      (plist-put project-plist 'test-prefix test-prefix))
    (puthash project-type project-plist
             projectile-project-types)))

(defun projectile-cmake-run-target (target)
  "Run CMake TARGET, generating build dir if needed."
  (interactive "sTarget: ")
  (let ((configure-cmd (format "cmake -E chdir %s cmake .."
                               projectile-build-dir))
        (build-cmd (format "cmake --build %s --target %s"
                           projectile-build-dir
                           target)))
    (compile (if (file-accessible-directory-p projectile-build-dir)
                 build-cmd
               (mkdir projectile-build-dir)
               (format "%s && %s" configure-cmd build-cmd)))))

(defun projectile-cmake-compile ()
  "Compile the current cmake project."
  (interactive)
  (projectile-cmake-run-target ""))

(defun projectile-cmake-test ()
  "Run the current cmake projects test suite."
  (interactive)
  (projectile-cmake-run-target "test"))

(defun projectile-meson-run-target (target)
  "Run meson TARGET, generating build dir if needed."
  (interactive "sTarget: ")
  (let* ((configure-cmd (format "meson %s" projectile-build-dir))
         (build-cmd (format "ninja -C %s %s" projectile-build-dir target)))
    (compile (if (file-accessible-directory-p projectile-build-dir)
                 build-cmd
               (format "%s && %s" configure-cmd build-cmd)))))

(defun projectile-meson-compile ()
  "Compile the current meson project."
  (interactive)
  (projectile-meson-run-target ""))

(defun projectile-meson-test ()
  "Run the current meson projects test suite."
  (interactive)
  (projectile-meson-run-target "test"))

(defun projectile-cabal-project-p ()
  "Check if a project contains *.cabal files but no stack.yaml file."
  (and (projectile-verify-file "*.cabal")
       (not (projectile-verify-file "stack.yaml"))))

(defun projectile-go-project-p ()
  "Check if a project contains Go source files."
  (cl-some
   (lambda (file)
     (string= (file-name-extension file) "go"))
   (projectile-current-project-files)))

(defcustom projectile-go-project-test-function #'projectile-go-project-p
  "Function to determine if project's type is go."
  :group 'projectile
  :type 'function)

(define-obsolete-variable-alias 'projectile-go-function 'projectile-go-project-test-function "0.15")

(projectile-register-project-type 'emacs-cask '("Cask")
                                  :compile "cask install")
(projectile-register-project-type 'rails-rspec '("Gemfile" "app" "lib" "db" "config" "spec")
                                  :compile "bundle exec rails server"
                                  :test "bundle exec rspec")
(projectile-register-project-type 'rails-test '("Gemfile" "app" "lib" "db" "config" "test")
                                  :compile "bundle exec rails server"
                                  :test "bundle exec rake test")
(projectile-register-project-type 'symfony '("composer.json" "app" "src" "vendor")
                                  :compile "app/console server:run"
                                  :test "phpunit -c app ")
(projectile-register-project-type 'ruby-rspec '("Gemfile" "lib" "spec")
                                  :compile "bundle exec rake"
                                  :test "bundle exec rspec")
(projectile-register-project-type 'ruby-test '("Gemfile" "lib" "test")
                                  :compile"bundle exec rake"
                                  :test "bundle exec rake test")
(projectile-register-project-type 'django '("manage.py")
                                  :compile "python manage.py runserver"
                                  :test "python manage.py test")
(projectile-register-project-type 'python-pip '("requirements.txt")
                                  :compile "python setup.by build"
                                  :test "python -m unittest discover")
(projectile-register-project-type 'python-pkg '("setup.py")
                                  :compile "python setup.py build"
                                  :test "python -m unittest discover")
(projectile-register-project-type 'python-tox '("tox.ini")
                                  :compile "tox -r --notest"
                                  :test "tox")
(projectile-register-project-type 'scons '("SConstruct")
                                  :compile "scons"
                                  :test "scons test")
(projectile-register-project-type 'maven '("pom.xml")
                                  :compile "mvn clean install"
                                  :test "mvn test")
(projectile-register-project-type 'gradle '("build.gradle")
                                  :compile "gradle build"
                                  :test "gradle test")
(projectile-register-project-type 'gradlew '("gradlew")
                                  :compile "./gradlew build"
                                  :test "./gradlew test")
(projectile-register-project-type 'grails '("application.properties" "grails-app")
                                  :compile "grails package"
                                  :test "grails test-app")
(projectile-register-project-type 'lein-test '("project.clj")
                                  :compile "lein compile"
                                  :test "lein test")
(projectile-register-project-type 'lein-midje '("project.clj" ".midje.clj")
                                  :compile "lein compile"
                                  :test "lein midje")
(projectile-register-project-type 'boot-clj '("build.boot")
                                  :compile "boot aot"
                                  :test "boot test")
(projectile-register-project-type 'rebar '("rebar.config")
                                  :compile "rebar"
                                  :test "rebar eunit")
(projectile-register-project-type 'sbt '("build.sbt")
                                  :compile "sbt compile"
                                  :test "sbt test")
(projectile-register-project-type 'make '("Makefile")
                                  :compile "make"
                                  :test "make test")
(projectile-register-project-type 'grunt '("Gruntfile.js")
                                  :compile "grunt"
                                  :test "grunt test")
(projectile-register-project-type 'gulp '("gulpfile.js")
                                  :compile "gulp"
                                  :test "gulp test")
(projectile-register-project-type 'haskell-stack '("stack.yaml")
                                  :compile "stack build"
                                  :test "stack build --test")
(projectile-register-project-type 'haskell-cabal #'projectile-cabal-project-p
                                  :compile "cabal build"
                                  :test "cabal test")
(projectile-register-project-type 'rust-cargo '("Cargo.toml")
                                  :compile "cargo build"
                                  :test "cargo test")
(projectile-register-project-type 'r '("DESCRIPTION")
                                  :compile "R CMD INSTALL --with-keep.source ."
                                  :test (concat "R CMD check -o " temporary-file-directory " ."))
(projectile-register-project-type 'go projectile-go-project-test-function
                                  :compile "go build ./..."
                                  :test "go test ./...")
(projectile-register-project-type 'racket '("info.rkt")
                                  :test "raco test .")
(projectile-register-project-type 'elixir '("mix.exs")
                                  :compile "mix compile"
                                  :test "mix test")
(projectile-register-project-type 'npm '("package.json")
                                  :compile "npm install"
                                  :test "npm test")
(projectile-register-project-type 'meson '("meson.build")
                                  :compile #'projectile-meson-compile
                                  :test #'projectile-meson-test)
(projectile-register-project-type 'cmake '("CMakeLists.txt")
                                  :compile #'projectile-cmake-compile
                                  :test #'projectile-cmake-test)
(projectile-register-project-type 'nix '("default.nix")
                                  :compile "nix-build"
                                  :test "nix-build")

(defvar-local projectile-project-type nil
  "Buffer local var for overriding the auto-detected project type.
Normally you'd set this from .dir-locals.el.")
(put 'projectile-project-type 'safe-local-variable #'symbolp)

(defun projectile-detect-project-type ()
  "Detect the type of the current project."
  (let ((project-type (cl-find-if
                       (lambda (project-type)
                         (let ((marker (plist-get (gethash project-type projectile-project-types) 'marker-files)))
                           (if (listp marker)
                               (and (projectile-verify-files marker) project-type)
                             (and (funcall marker) project-type))))
                       (projectile-hash-keys projectile-project-types))))
    (when project-type
      (puthash (projectile-project-root) project-type projectile-project-type-cache))
    project-type))

(defun projectile-project-type ()
  "Determine the project's type based on its structure."
  (if projectile-project-type
      projectile-project-type
    (let ((project-root (ignore-errors (projectile-project-root))))
      (if project-root
          (or (gethash project-root projectile-project-type-cache)
              (projectile-detect-project-type)
              'generic)
        'generic))))

;;;###autoload
(defun projectile-project-info ()
  "Display info for current project."
  (interactive)
  (message "Project dir: %s ## Project VCS: %s ## Project type: %s"
           (projectile-project-root)
           (projectile-project-vcs)
           (projectile-project-type)))

(defun projectile-verify-files (files)
  "Check whether all FILES exist in the current project."
  (cl-every 'projectile-verify-file files))

(defun projectile-verify-file (file)
  "Check whether FILE exists in the current project.
Expands wildcards using `file-expand-wildcards' before checking."
  (file-expand-wildcards (projectile-expand-root file)))

(defun projectile-project-vcs (&optional project-root)
  "Determine the VCS used by the project if any.
PROJECT-ROOT is the targeted directory.  If nil, use
`projectile-project-root'."
  (or project-root (setq project-root (projectile-project-root)))
  (cond
   ((projectile-file-exists-p (expand-file-name ".git" project-root)) 'git)
   ((projectile-file-exists-p (expand-file-name ".hg" project-root)) 'hg)
   ((projectile-file-exists-p (expand-file-name ".fslckout" project-root)) 'fossil)
   ((projectile-file-exists-p (expand-file-name "_FOSSIL_" project-root)) 'fossil)
   ((projectile-file-exists-p (expand-file-name ".bzr" project-root)) 'bzr)
   ((projectile-file-exists-p (expand-file-name "_darcs" project-root)) 'darcs)
   ((projectile-file-exists-p (expand-file-name ".svn" project-root)) 'svn)
   ((projectile-locate-dominating-file project-root ".git") 'git)
   ((projectile-locate-dominating-file project-root ".hg") 'hg)
   ((projectile-locate-dominating-file project-root ".fslckout") 'fossil)
   ((projectile-locate-dominating-file project-root "_FOSSIL_") 'fossil)
   ((projectile-locate-dominating-file project-root ".bzr") 'bzr)
   ((projectile-locate-dominating-file project-root "_darcs") 'darcs)
   ((projectile-locate-dominating-file project-root ".svn") 'svn)
   (t 'none)))

(defun projectile--test-name-for-impl-name (impl-file-path)
  "Determine the name of the test file for IMPL-FILE-PATH."
  (let* ((project-type (projectile-project-type))
         (impl-file-name (file-name-sans-extension (file-name-nondirectory impl-file-path)))
         (impl-file-ext (file-name-extension impl-file-path))
         (test-prefix (funcall projectile-test-prefix-function project-type))
         (test-suffix (funcall projectile-test-suffix-function project-type)))
    (cond
     (test-prefix (concat test-prefix impl-file-name "." impl-file-ext))
     (test-suffix (concat impl-file-name test-suffix "." impl-file-ext))
     (t (error "Project type `%s' not supported!" project-type)))))

(defun projectile-create-test-file-for (impl-file-path)
  "Create a test file for IMPL-FILE-PATH."
  (let* ((test-file (projectile--test-name-for-impl-name impl-file-path))
         (project-root (projectile-project-root))
         (relative-dir (file-name-directory (file-relative-name impl-file-path project-root)))
         (test-dir (expand-file-name (replace-regexp-in-string "src/" "test/" relative-dir) project-root))
         (test-path (expand-file-name test-file test-dir)))
    (unless (file-exists-p test-path)
      (progn (unless (file-exists-p test-dir)
               (make-directory test-dir :create-parents))
             test-path))))

(defcustom projectile-create-missing-test-files nil
  "During toggling, if non-nil enables creating test files if not found.

When not-nil, every call to projectile-find-implementation-or-test-*
creates test files if not found on the file system.  Defaults to nil.
It assumes the test/ folder is at the same level as src/."
  :group 'projectile
  :type 'boolean)

(defun projectile-find-implementation-or-test (file-name)
  "Given a FILE-NAME return the matching implementation or test filename.

If `projectile-create-missing-test-files' is non-nil, create the missing
test file."
  (unless file-name (error "The current buffer is not visiting a file"))
  (if (projectile-test-file-p file-name)
      ;; find the matching impl file
      (let ((impl-file (projectile-find-matching-file file-name)))
        (if impl-file
            (projectile-expand-root impl-file)
          (error
           "No matching source file found for project type `%s'"
           (projectile-project-type))))
    ;; find the matching test file
    (let ((test-file (projectile-find-matching-test file-name)))
      (if test-file
          (projectile-expand-root test-file)
        (if projectile-create-missing-test-files
            (projectile-create-test-file-for file-name)
          (error "No matching test file found for project type `%s'"
                 (projectile-project-type)))))))

;;;###autoload
(defun projectile-find-implementation-or-test-other-window ()
  "Open matching implementation or test file in other window."
  (interactive)
  (find-file-other-window
   (projectile-find-implementation-or-test (buffer-file-name))))

;;;###autoload
(defun projectile-find-implementation-or-test-other-frame ()
  "Open matching implementation or test file in other frame."
  (interactive)
  (find-file-other-frame
   (projectile-find-implementation-or-test (buffer-file-name))))

;;;###autoload
(defun projectile-toggle-between-implementation-and-test ()
  "Toggle between an implementation file and its test file."
  (interactive)
  (find-file
   (projectile-find-implementation-or-test (buffer-file-name))))


(defun projectile-project-type-attribute (project-type key &optional default-value)
  "Return the value of some PROJECT-TYPE attribute identified by KEY.
Fallback to DEFAULT-VALUE for missing attributes."
  (let ((project (gethash project-type projectile-project-types)))
    (if (and project (plist-member project key))
        (plist-get project key)
      default-value)))

(defun projectile-test-prefix (project-type)
  "Find default test files prefix based on PROJECT-TYPE."
  (cl-flet ((prefix (&optional pfx)
                    (projectile-project-type-attribute project-type 'test-prefix pfx)))
      (cond
       ((member project-type '(django python-pip python-pkg python-tox))  (prefix "test_"))
       ((member project-type '(emacs-cask)) (prefix "test-"))
       ((member project-type '(lein-midje)) (prefix "t_"))
       (t (prefix)))))

(defun projectile-test-suffix (project-type)
  "Find default test files suffix based on PROJECT-TYPE."
  (cl-flet ((suffix (&optional sfx)
                    (projectile-project-type-attribute project-type 'test-suffix sfx)))
    (cond
     ((member project-type '(rebar)) (suffix "_SUITE"))
     ((member project-type '(emacs-cask)) (suffix "-test"))
     ((member project-type '(rails-rspec ruby-rspec)) (suffix "_spec"))
     ((member project-type '(rails-test ruby-test lein-test boot-clj go elixir)) (suffix "_test"))
     ((member project-type '(scons)) (suffix "test"))
     ((member project-type '(maven symfony)) (suffix "Test"))
     ((member project-type '(gradle gradlew grails)) (suffix "Spec"))
     ((member project-type '(sbt)) (suffix "Spec"))
     (t (suffix)))))

(defun projectile-dirname-matching-count (a b)
  "Count matching dirnames ascending file paths."
  (setq a (reverse (split-string (or (file-name-directory a) "") "/" t))
        b (reverse (split-string (or (file-name-directory b) "") "/" t)))
  (let ((common 0))
    (while (and a b (string-equal (pop a) (pop b)))
      (setq common (1+ common)))
    common))

(defun projectile-group-file-candidates (file candidates)
  "Group file candidates by dirname matching count."
  (cl-sort (copy-sequence
            (let (value result)
              (while (setq value (pop candidates))
                (let* ((key (projectile-dirname-matching-count file value))
                       (kv (assoc key result)))
                  (if kv
                      (setcdr kv (cons value (cdr kv)))
                    (push (list key value) result))))
              (mapcar (lambda (x)
                        (cons (car x) (nreverse (cdr x))))
                      (nreverse result))))
           (lambda (a b) (> (car a) (car b)))))

(defun projectile-find-matching-test (file)
  "Compute the name of the test matching FILE."
  (let* ((basename (file-name-nondirectory (file-name-sans-extension file)))
         (test-prefix (funcall projectile-test-prefix-function (projectile-project-type)))
         (test-suffix (funcall projectile-test-suffix-function (projectile-project-type)))
         (candidates
          (cl-remove-if-not
           (lambda (current-file)
             (let ((name (file-name-nondirectory
                          (file-name-sans-extension current-file))))
               (or (when test-prefix
                     (string-equal name (concat test-prefix basename)))
                   (when test-suffix
                     (string-equal name (concat basename test-suffix))))))
           (projectile-current-project-files))))
    (cond
     ((null candidates) nil)
     ((= (length candidates) 1) (car candidates))
     (t (let ((grouped-candidates (projectile-group-file-candidates file candidates)))
          (if (= (length (car grouped-candidates)) 2)
              (car (last (car grouped-candidates)))
            (projectile-completing-read
             "Switch to: "
             (apply 'append (mapcar 'cdr grouped-candidates)))))))))

(defun projectile-find-matching-file (test-file)
  "Compute the name of a file matching TEST-FILE."
  (let* ((basename (file-name-nondirectory (file-name-sans-extension test-file)))
         (test-prefix (funcall projectile-test-prefix-function (projectile-project-type)))
         (test-suffix (funcall projectile-test-suffix-function (projectile-project-type)))
         (candidates
          (cl-remove-if-not
           (lambda (current-file)
             (let ((name (file-name-nondirectory
                          (file-name-sans-extension current-file))))
               (or (when test-prefix
                     (string-equal (concat test-prefix name) basename))
                   (when test-suffix
                     (string-equal (concat name test-suffix) basename)))))
           (projectile-current-project-files))))
    (cond
     ((null candidates) nil)
     ((= (length candidates) 1) (car candidates))
     (t (let ((grouped-candidates (projectile-group-file-candidates test-file candidates)))
          (if (= (length (car grouped-candidates)) 2)
              (car (last (car grouped-candidates)))
            (projectile-completing-read
             "Switch to: "
             (apply 'append (mapcar 'cdr grouped-candidates)))))))))

(defun projectile-grep-default-files ()
  "Try to find a default pattern for `projectile-grep'.
This is a subset of `grep-read-files', where either a matching entry from
`grep-files-aliases' or file name extension pattern is returned."
  (when buffer-file-name
    (let* ((fn (file-name-nondirectory buffer-file-name))
           (default-alias
             (let ((aliases (remove (assoc "all" grep-files-aliases)
                                    grep-files-aliases))
                   alias)
               (while aliases
                 (setq alias (car aliases)
                       aliases (cdr aliases))
                 (if (string-match (mapconcat
                                    #'wildcard-to-regexp
                                    (split-string (cdr alias) nil t)
                                    "\\|")
                                   fn)
                     (setq aliases nil)
                   (setq alias nil)))
               (cdr alias)))
           (default-extension
             (let ((ext (file-name-extension fn)))
               (and ext (concat "*." ext)))))
      (or default-alias default-extension))))

(defun projectile--globally-ignored-file-suffixes-glob ()
  "Return ignored file suffixes as a list of glob patterns."
  (mapcar (lambda (pat) (concat "*" pat)) projectile-globally-ignored-file-suffixes))

(defun projectile--read-search-string-with-default (prefix-label)
  (let* ((prefix-label (projectile-prepend-project-name prefix-label))
         (default-value (projectile-symbol-or-selection-at-point))
         (default-label (if (or (not default-value)
                                (string= default-value ""))
                            ""
                          (format " (default %s)" default-value))))
    (read-string (format "%s%s: " prefix-label default-label) nil nil default-value)))

;;;###autoload
(defun projectile-grep (&optional regexp arg)
  "Perform rgrep in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp."
  (interactive "i\nP")
  (require 'grep) ;; for `rgrep'
  (let* ((roots (projectile-get-project-directories))
         (search-regexp (or regexp
                            (projectile--read-search-string-with-default "Grep for")))
         (files (and arg (or (and (equal current-prefix-arg '-)
                                  (projectile-grep-default-files))
                             (read-string (projectile-prepend-project-name "Grep in: ")
                                          (projectile-grep-default-files))))))
    (dolist (root-dir roots)
      (require 'vc-git) ;; for `vc-git-grep'
      ;; in git projects users have the option to use `vc-git-grep' instead of `rgrep'
      (if (and (eq (projectile-project-vcs) 'git)
               projectile-use-git-grep
               (fboundp 'vc-git-grep))
          (vc-git-grep search-regexp (or files "") root-dir)
        ;; paths for find-grep should relative and without trailing /
        (let ((grep-find-ignored-directories
               (cl-union (mapcar (lambda (f) (directory-file-name (file-relative-name f root-dir)))
                                 (projectile-ignored-directories))
                         grep-find-ignored-directories))
              (grep-find-ignored-files
               (cl-union (append (mapcar (lambda (file)
                                           (file-relative-name file root-dir))
                                         (projectile-ignored-files))
                                 (projectile--globally-ignored-file-suffixes-glob))
                         grep-find-ignored-files)))
          (grep-compute-defaults)
          (rgrep search-regexp (or files "* .*") root-dir))))
    (run-hooks 'projectile-grep-finished-hook)))

;;;###autoload
(defun projectile-ag (search-term &optional arg)
  "Run an ag search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression."
  (interactive
   (list (projectile--read-search-string-with-default
          (format "Ag %ssearch for" (if current-prefix-arg "regexp " "")))
         current-prefix-arg))
  (if (require 'ag nil 'noerror)
      (let ((ag-command (if arg 'ag-regexp 'ag))
            (ag-ignore-list (delq nil
                                  (delete-dups
                                   (append
                                    ag-ignore-list
                                    (projectile--globally-ignored-file-suffixes-glob)
                                    ;; ag supports git ignore files directly
                                    (unless (eq (projectile-project-vcs) 'git)
                                      (append (projectile-ignored-files-rel)
                                              (projectile-ignored-directories-rel)
                                              grep-find-ignored-files
                                              grep-find-ignored-directories))))))
            ;; reset the prefix arg, otherwise it will affect the ag-command
            (current-prefix-arg nil))
        (funcall ag-command search-term (projectile-project-root)))
    (error "Package 'ag' is not available")))

(defun projectile-tags-exclude-patterns ()
  "Return a string with exclude patterns for ctags."
  (mapconcat (lambda (pattern) (format "--exclude=\"%s\""
                                       (directory-file-name pattern)))
             (projectile-ignored-directories-rel) " "))

;;;###autoload
(defun projectile-regenerate-tags ()
  "Regenerate the project's [e|g]tags."
  (interactive)
  (if (and (boundp 'ggtags-mode)
           (memq projectile-tags-backend '(auto ggtags)))
      (progn
        (let* ((ggtags-project-root (projectile-project-root))
               (default-directory ggtags-project-root))
          (ggtags-ensure-project)
          (ggtags-update-tags t)))
    (let* ((project-root (projectile-project-root))
           (tags-exclude (projectile-tags-exclude-patterns))
           (default-directory project-root)
           (tags-file (expand-file-name projectile-tags-file-name))
           (command (format projectile-tags-command tags-file tags-exclude))
           shell-output exit-code)
      (with-temp-buffer
        (setq exit-code
              (call-process-shell-command command nil (current-buffer))
              shell-output (projectile-trim-string
                            (buffer-substring (point-min) (point-max)))))
      (unless (zerop exit-code)
        (error shell-output))
      (visit-tags-table tags-file)
      (message "Regenerated %s" tags-file))))

(defun projectile-visit-project-tags-table ()
  "Visit the current project's tags table."
  (when (projectile-project-p)
    (let ((tags-file (projectile-expand-root projectile-tags-file-name)))
      (when (file-exists-p tags-file)
        (with-demoted-errors "Error loading tags-file: %s"
          (visit-tags-table tags-file t))))))

(defun projectile-determine-find-tag-fn ()
  "Determine which function to use for a call to `projectile-find-tag'."
  (or
   (cond
    ((eq projectile-tags-backend 'auto)
     (cond
      ((fboundp 'ggtags-find-tag-dwim)
       'ggtags-find-tag-dwim)
      ((fboundp 'xref-find-definitions)
       'xref-find-definitions)
      ((fboundp 'etags-select-find-tag)
       'etags-select-find-tag)))
    ((eq projectile-tags-backend 'xref)
     (when (fboundp 'xref-find-definitions)
       'xref-find-definitions))
    ((eq projectile-tags-backend 'ggtags)
     (when (fboundp 'ggtags-find-tag-dwim)
       'ggtags-find-tag-dwim))
    ((eq projectile-tags-backend 'etags-select)
     (when (fboundp 'etags-select-find-tag)
       'etags-select-find-tag)))
   'find-tag))

;;;###autoload
(defun projectile-find-tag ()
  "Find tag in project."
  (interactive)
  (projectile-visit-project-tags-table)
  ;; Auto-discover the user's preference for tags
  (let ((find-tag-fn (projectile-determine-find-tag-fn)))
    (call-interactively find-tag-fn)))

(defmacro projectile-with-default-dir (dir &rest body)
  "Invoke in DIR the BODY."
  (declare (debug t) (indent 1))
  `(let ((default-directory ,dir))
     ,@body))

;;;###autoload
(defun projectile-run-command-in-root ()
  "Invoke `execute-extended-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'execute-extended-command)))

;;;###autoload
(defun projectile-run-shell-command-in-root ()
  "Invoke `shell-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'shell-command)))

;;;###autoload
(defun projectile-run-async-shell-command-in-root ()
  "Invoke `async-shell-command' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (call-interactively 'async-shell-command)))

;;;###autoload
(defun projectile-run-shell ()
  "Invoke `shell' in the project's root."
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (shell (concat "*shell " (projectile-project-name) "*"))))

;;;###autoload
(defun projectile-run-eshell ()
  "Invoke `eshell' in the project's root."
  (interactive)
  (let ((eshell-buffer-name (concat "*eshell " (projectile-project-name) "*")))
    (projectile-with-default-dir (projectile-project-root)
      (eshell))))

;;;###autoload
(defun projectile-run-term (program)
  "Invoke `term' in the project's root."
  (interactive (list nil))
  (let* ((term (concat "term " (projectile-project-name)))
         (buffer (concat "*" term "*")))
    (unless (get-buffer buffer)
      (require 'term)
      (let ((program (or program
                         (read-from-minibuffer "Run program: "
                                               (or explicit-shell-file-name
                                                   (getenv "ESHELL")
                                                   (getenv "SHELL")
                                                   "/bin/sh")))))
        (projectile-with-default-dir (projectile-project-root)
          (set-buffer (make-term term program))
          (term-mode)
          (term-char-mode))))
    (switch-to-buffer buffer)))

(defun projectile-files-in-project-directory (directory)
  "Return a list of files in DIRECTORY."
  (let ((dir (file-relative-name (expand-file-name directory)
                                 (projectile-project-root))))
    (cl-remove-if-not
     (lambda (f) (string-prefix-p dir f))
     (projectile-current-project-files))))

(defun projectile-unixy-system-p ()
  "Check to see if unixy text utilities are installed."
  (cl-every
   (lambda (x) (executable-find x))
   '("grep" "cut" "uniq")))

(defun projectile-files-from-cmd (cmd directory)
  "Use a grep-like CMD to search for files within DIRECTORY.

CMD should include the necessary search params and should output
equivalently to grep -HlI (only unique matching filenames).
Returns a list of expanded filenames."
  (let ((default-directory directory))
    (mapcar (lambda (str)
              (concat directory
                      (if (string-prefix-p "./" str)
                          (substring str 2)
                        str)))
            (split-string
             (projectile-trim-string
              (shell-command-to-string cmd))
             "\n+"
             t))))

(defun projectile-files-with-string (string directory)
  "Return a list of all files containing STRING in DIRECTORY.

Tries to use ag, ack, git-grep, and grep in that order.  If those
are impossible (for instance on Windows), returns a list of all
files in the project."
  (if (projectile-unixy-system-p)
      (let* ((search-term (shell-quote-argument string))
             (cmd (cond ((executable-find "ag")
                         (concat "ag --literal --nocolor --noheading -l -- "
                                 search-term))
                        ((executable-find "ack")
                         (concat "ack --literal --noheading --nocolor -l -- "
                                 search-term))
                        ((and (executable-find "git")
                              (eq (projectile-project-vcs) 'git))
                         (concat "git grep -HlI " search-term))
                        (t
                         ;; -r: recursive
                         ;; -H: show filename for each match
                         ;; -l: show only file names with matches
                         ;; -I: no binary files
                         (format "grep -rHlI %s ." search-term)))))
        (projectile-files-from-cmd cmd directory))
    ;; we have to reject directories as a workaround to work with git submodules
    (cl-remove-if
     #'file-directory-p
     (mapcar #'projectile-expand-root (projectile-dir-files directory)))))

;;;###autoload
(defun projectile-replace (&optional arg)
  "Replace literal string in project using non-regexp `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement."
  (interactive "P")
  (let* ((old-text (read-string
                    (projectile-prepend-project-name "Replace: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace %s with: " old-text))))
         (directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace in directory: "))
                      (projectile-project-root)))
         (files (projectile-files-with-string old-text directory)))
    ;; Adapted from `tags-query-replace' for literal strings (not regexp)
    (setq tags-loop-scan `(let ,(unless (equal old-text (downcase old-text))
                                  '((case-fold-search nil)))
                            (if (search-forward ',old-text nil t)
                                ;; When we find a match, move back to
                                ;; the beginning of it so
                                ;; perform-replace will see it.
                                (goto-char (match-beginning 0))))
          tags-loop-operate `(perform-replace ',old-text ',new-text t nil nil
                                              nil multi-query-replace-map))
    (tags-loop-continue (or (cons 'list files) t))))

;;;###autoload
(defun projectile-replace-regexp (&optional arg)
  "Replace a regexp in the project using `tags-query-replace'.

With a prefix argument ARG prompts you for a directory on which
to run the replacement."
  (interactive "P")
  (let* ((old-text (read-string
                    (projectile-prepend-project-name "Replace regexp: ")
                    (projectile-symbol-or-selection-at-point)))
         (new-text (read-string
                    (projectile-prepend-project-name
                     (format "Replace regexp %s with: " old-text))))
         (directory (if arg
                        (file-name-as-directory
                         (read-directory-name "Replace regexp in directory: "))
                      (projectile-project-root)))
         (files
          ;; We have to reject directories as a workaround to work with git submodules.
          ;;
          ;; We can't narrow the list of files with
          ;; `projectile-files-with-string' because those regexp tools
          ;; don't support Emacs regular expressions.
          (cl-remove-if
           #'file-directory-p
           (mapcar #'projectile-expand-root (projectile-dir-files directory)))))
    (tags-query-replace old-text new-text nil (cons 'list files))))

(defun projectile-symbol-or-selection-at-point ()
  "Get the symbol or selected text at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (projectile-symbol-at-point)))

(defun projectile-symbol-at-point ()
  "Get the symbol at point and strip its properties."
  (substring-no-properties (or (thing-at-point 'symbol) "")))

;;;###autoload
(defun projectile-kill-buffers ()
  "Kill all project buffers."
  (interactive)
  (let ((name (projectile-project-name))
        (buffers (projectile-project-buffers)))
    (if (yes-or-no-p
         (format "Are you sure you want to kill %d buffer(s) for '%s'? "
                 (length buffers) name))
        ;; we take care not to kill indirect buffers directly
        ;; as we might encounter them after their base buffers are killed
        (mapc #'kill-buffer (cl-remove-if 'buffer-base-buffer buffers)))))

;;;###autoload
(defun projectile-save-project-buffers ()
  "Save all project buffers."
  (interactive)
  (dolist (buf (projectile-project-buffers))
    (with-current-buffer buf
      (when buffer-file-name
        (save-buffer)))))

;;;###autoload
(defun projectile-dired ()
  "Open `dired' at the root of the project."
  (interactive)
  (dired (projectile-project-root)))

;;;###autoload
(defun projectile-dired-other-window ()
  "Open `dired'  at the root of the project in another window."
  (interactive)
  (dired-other-window (projectile-project-root)))

;;;###autoload
(defun projectile-dired-other-frame ()
  "Open `dired' at the root of the project in another frame."
  (interactive)
  (dired-other-frame (projectile-project-root)))

;;;###autoload
(defun projectile-vc (&optional project-root)
  "Open `vc-dir' at the root of the project.

For git projects `magit-status-internal' is used if available.
For hg projects `monky-status' is used if available."
  (interactive)
  (or project-root (setq project-root (projectile-project-root)))
  (let ((vcs (projectile-project-vcs project-root)))
    (cl-case vcs
      (git
       (cond ((fboundp 'magit-status-internal)
              (magit-status-internal project-root))
             ((fboundp 'magit-status)
              (with-no-warnings (magit-status project-root)))
             (t
              (vc-dir project-root))))
      (hg
       (if (fboundp 'monky-status)
           (monky-status project-root)
         (vc-dir project-root)))
      (t (vc-dir project-root)))))

;;;###autoload
(defun projectile-recentf ()
  "Show a list of recently visited files in a project."
  (interactive)
  (if (boundp 'recentf-list)
      (find-file (projectile-expand-root
                  (projectile-completing-read
                   "Recently visited files: "
                   (projectile-recentf-files))))
    (message "recentf is not enabled")))

(defun projectile-recentf-files ()
  "Return a list of recently visited files in a project."
  (and (boundp 'recentf-list)
       (let ((project-root (projectile-project-root)))
         (mapcar
          (lambda (f) (file-relative-name f project-root))
          (cl-remove-if-not
           (lambda (f) (string-prefix-p project-root f))
           recentf-list)))))

(defun projectile-serialize-cache ()
  "Serializes the memory cache to the hard drive."
  (projectile-serialize projectile-projects-cache projectile-cache-file))

(defvar projectile-compilation-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last compilation command used on them.")

(defvar projectile-test-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last test command used on them.")

(defvar projectile-run-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last run command used on them.")

(defvar projectile-project-compilation-cmd nil
  "The command to use with `projectile-compile-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-compilation-dir nil
  "The directory to use with `projectile-compile-project'.
The directory path is relative to the project root.
Should be set via .dir-locals.el.")

(defvar projectile-project-test-cmd nil
  "The command to use with `projectile-test-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defvar projectile-project-run-cmd nil
  "The command to use with `projectile-run-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defun projectile-default-compilation-command (project-type)
  "Retrieve default compilation command for PROJECT-TYPE."
  (plist-get (gethash project-type projectile-project-types) 'compile-command))

(defun projectile-default-test-command (project-type)
  "Retrieve default test command for PROJECT-TYPE."
  (plist-get (gethash project-type projectile-project-types) 'test-command))

(defun projectile-default-run-command (project-type)
  "Retrieve default run command for PROJECT-TYPE."
  (plist-get (gethash project-type projectile-project-types) 'run-command))

(defun projectile-compilation-command (compile-dir)
  "Retrieve the compilation command for COMPILE-DIR."
  (or (gethash compile-dir projectile-compilation-cmd-map)
      projectile-project-compilation-cmd
      (projectile-default-compilation-command (projectile-project-type))))

(defun projectile-test-command (project)
  "Retrieve the test command for PROJECT."
  (or (gethash project projectile-test-cmd-map)
      projectile-project-test-cmd
      (projectile-default-test-command (projectile-project-type))))

(defun projectile-run-command (project)
  "Retrieve the run command for PROJECT."
  (or (gethash project projectile-run-cmd-map)
      projectile-project-run-cmd
      (projectile-default-run-command (projectile-project-type))))

(defun projectile-read-command (prompt command)
  "Adapted from `compilation-read-command'."
  (read-shell-command prompt command
                      (if (equal (car compile-history) command)
                          '(compile-history . 1)
                        'compile-history)))

(defun projectile-compilation-dir ()
  "Choose the directory to use for project compilation."
  (if projectile-project-compilation-dir
      (file-truename
       (concat (file-name-as-directory (projectile-project-root))
               (file-name-as-directory projectile-project-compilation-dir)))
    (projectile-project-root)))

(defun projectile-maybe-read-command (arg default-cmd prompt)
  "Prompt user for command unless DEFAULT-CMD is an Elisp function."
  (if (and (or (stringp default-cmd) (null default-cmd))
           (or compilation-read-command arg))
      (projectile-read-command prompt default-cmd)
    default-cmd))

(defun projectile-run-compilation (cmd)
  "Run external or Elisp compilation command CMD."
  (if (functionp cmd)
      (funcall cmd)
    (compile cmd)))

;;;###autoload
(defun projectile-compile-project (arg &optional dir)
  "Run project compilation command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (default-directory (or dir (projectile-compilation-dir)))
         (default-cmd (projectile-compilation-command default-directory))
         (compilation-cmd (projectile-maybe-read-command arg default-cmd "Compile command: ")))
    (puthash default-directory compilation-cmd projectile-compilation-cmd-map)
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer)
                                                      project-root)))
    (projectile-run-compilation compilation-cmd)))

(defadvice compilation-find-file (around projectile-compilation-find-file)
  "Try to find a buffer for FILENAME, if we cannot find it,
fallback to the original function."
  (let ((filename (ad-get-arg 1))
        full-filename)
    (ad-set-arg 1
                (or
                 (if (file-exists-p (expand-file-name filename))
                     filename)
                 ;; Try to find the filename using projectile
                 (and (projectile-project-p)
                      (let ((root (projectile-project-root))
                            (dirs (cons "" (projectile-current-project-dirs))))
                        (when (setq full-filename
                                    (car (cl-remove-if-not
                                          #'file-exists-p
                                          (mapcar
                                           (lambda (f)
                                             (expand-file-name
                                              filename
                                              (expand-file-name f root)))
                                           dirs))))
                          full-filename)))
                 ;; Fall back to the old argument
                 filename))
    ad-do-it))

;; TODO - factor this duplication out
;;;###autoload
(defun projectile-test-project (arg)
  "Run project test command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (default-cmd (projectile-test-command project-root))
         (test-cmd (projectile-maybe-read-command arg default-cmd "Test command: "))
         (default-directory project-root))
    (puthash project-root test-cmd projectile-test-cmd-map)
    (save-some-buffers (not compilation-ask-about-save)
                       (lambda ()
                         (projectile-project-buffer-p (current-buffer)
                                                      project-root)))
    (projectile-run-compilation test-cmd)))

;;;###autoload
(defun projectile-run-project (arg)
  "Run project run command.

Normally you'll be prompted for a compilation command, unless
variable `compilation-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (default-cmd (projectile-run-command project-root))
         (run-cmd (projectile-maybe-read-command arg default-cmd "Run command: "))
         (default-directory project-root))
    (puthash project-root run-cmd projectile-run-cmd-map)
    (projectile-run-compilation run-cmd)))

(defun projectile-open-projects ()
  "Return a list of all open projects.
An open project is a project with any open buffers."
  (delete-dups
   (delq nil
         (mapcar (lambda (buffer)
                   (with-current-buffer buffer
                     (when (projectile-project-p)
                       (abbreviate-file-name (projectile-project-root)))))
                 (buffer-list)))))

(defun projectile--remove-current-project (projects)
  "Remove the current project (if any) from the list of PROJECTS."
  (if (projectile-project-p)
      (projectile-difference projects
                             (list (abbreviate-file-name (projectile-project-root))))
    projects))

(defun projectile-relevant-known-projects ()
  "Return a list of known projects except the current one (if present)."
  (projectile--remove-current-project projectile-known-projects))

(defun projectile-relevant-open-projects ()
  "Return a list of open projects except the current one (if present)."
  (projectile--remove-current-project (projectile-open-projects)))

;;;###autoload
(defun projectile-switch-project (&optional arg)
  "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (let ((projects (projectile-relevant-known-projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: " projects
         :action (lambda (project)
                   (projectile-switch-project-by-name project arg)))
      (user-error "There are no known projects"))))

;;;###autoload
(defun projectile-switch-open-project (&optional arg)
  "Switch to a project we have currently opened.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (let ((projects (projectile-relevant-open-projects)))
    (if projects
        (projectile-switch-project-by-name
         (projectile-completing-read "Switch to open project: " projects)
         arg)
      (user-error "There are no open projects"))))

(defun projectile-switch-project-by-name (project-to-switch &optional arg)
  "Switch to project by project name PROJECT-TO-SWITCH.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (let ((switch-project-action (if arg
                                   'projectile-commander
                                 projectile-switch-project-action)))
    (run-hooks 'projectile-before-switch-project-hook)
    (let ((default-directory project-to-switch))
      ;; use a temporary buffer to load PROJECT-TO-SWITCH's dir-locals before calling SWITCH-PROJECT-ACTION
      (with-temp-buffer
        (hack-dir-local-variables-non-file-buffer))
      ;; Normally the project name is determined from the current
      ;; buffer. However, when we're switching projects, we want to
      ;; show the name of the project being switched to, rather than
      ;; the current project, in the minibuffer. This is a simple hack
      ;; to tell the `projectile-project-name' function to ignore the
      ;; current buffer and the caching mechanism, and just return the
      ;; value of the `projectile-project-name' variable.  We also
      ;; need to ignore the cached project-root value otherwise we end
      ;; up still showing files from the current project rather than
      ;; the new project
      (let ((projectile-cached-project-root nil)
            (projectile-project-name (funcall projectile-project-name-function
                                              project-to-switch)))
        (funcall switch-project-action)))
    (run-hooks 'projectile-after-switch-project-hook)))

;;;###autoload
(defun projectile-find-file-in-directory (&optional directory)
  "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in."
  (interactive "DFind file in directory: ")
  (let ((default-directory directory)
        (projectile-require-project-root nil))
    (if (projectile-project-p)
        ;; target directory is in a project
        (let ((file (projectile-completing-read "Find file: "
                                                (projectile-dir-files directory))))
          (find-file (expand-file-name file (projectile-project-root)))
          (run-hooks 'projectile-find-file-hook))
      ;; target directory is not in a project
      (projectile-find-file))))

(defun projectile-all-project-files ()
  "Get a list of all files in all projects."
  (cl-mapcan
   (lambda (project)
     (when (file-exists-p project)
       (let ((default-directory project)
             (projectile-cached-project-root nil))
         (mapcar (lambda (file)
                   (expand-file-name file project))
                 (projectile-current-project-files)))))
   projectile-known-projects))

;;;###autoload
(defun projectile-find-file-in-known-projects ()
  "Jump to a file in any of the known projects."
  (interactive)
  (let ((projectile-require-project-root nil))
    (find-file (projectile-completing-read "Find file in projects: " (projectile-all-project-files)))))

(defcustom projectile-after-switch-project-hook nil
  "Hooks run right after project is switched."
  :group 'projectile
  :type 'hook)

(defcustom projectile-before-switch-project-hook nil
  "Hooks run when right before project is switched."
  :group 'projectile
  :type 'hook)

(defun projectile-keep-project-p (project)
  "Determine whether we should cleanup (remove) PROJECT or not.

It handles the case of remote projects as well.
See `projectile-cleanup-known-projects'."
  ;; Taken from from `recentf-keep-default-predicate'
  (cond
   ((file-remote-p project nil t) (file-readable-p project))
   ((file-remote-p project))
   ((file-readable-p project))))

;;;###autoload
(defun projectile-cleanup-known-projects ()
  "Remove known projects that don't exist anymore."
  (interactive)
  (projectile-merge-known-projects)
  (let ((projects-kept (cl-remove-if-not #'projectile-keep-project-p projectile-known-projects))
        (projects-removed (cl-remove-if #'projectile-keep-project-p projectile-known-projects)))
    (setq projectile-known-projects projects-kept)
    (projectile-merge-known-projects)
    (if projects-removed
        (message "Projects removed: %s"
                 (mapconcat #'identity projects-removed ", "))
      (message "No projects needed to be removed."))))

;;;###autoload
(defun projectile-clear-known-projects ()
  "Clear both `projectile-known-projects' and `projectile-known-projects-file'."
  (interactive)
  (setq projectile-known-projects nil)
  (projectile-save-known-projects))

;;;###autoload
(defun projectile-remove-known-project (&optional project)
  "Remove PROJECT from the list of known projects."
  (interactive (list (projectile-completing-read
                      "Remove from known projects: " projectile-known-projects
                      :action 'projectile-remove-known-project)))
  (unless (called-interactively-p 'any)
    (setq projectile-known-projects
          (cl-remove-if
           (lambda (proj) (string= project proj))
           projectile-known-projects))
    (projectile-merge-known-projects)
    (when projectile-verbose
      (message "Project %s removed from the list of known projects." project))))

;;;###autoload
(defun projectile-remove-current-project-from-known-projects ()
  "Remove the current project from the list of known projects."
  (interactive)
  (projectile-remove-known-project (abbreviate-file-name (projectile-project-root))))

(defun projectile-ignored-projects ()
  "A list of projects that should not be save in `projectile-known-projects'."
  (mapcar #'file-truename projectile-ignored-projects))

(defun projectile-ignored-project-p (project-root)
  "Return t if PROJECT-ROOT should not be added to `projectile-known-projects'."
  (or (member project-root (projectile-ignored-projects))
      (and (functionp projectile-ignored-project-function)
           (funcall projectile-ignored-project-function project-root))))

(defun projectile-add-known-project (project-root)
  "Add PROJECT-ROOT to the list of known projects."
  (interactive (list (read-directory-name "Add to known projects: ")))
  (unless (projectile-ignored-project-p project-root)
    (setq projectile-known-projects
          (delete-dups
           (cons (abbreviate-file-name project-root)
                 projectile-known-projects)))))

(defun projectile-load-known-projects ()
  "Load saved projects from `projectile-known-projects-file'.
Also set `projectile-known-projects'."
  (setq projectile-known-projects
        (projectile-unserialize projectile-known-projects-file))
  (setq projectile-known-projects-on-file
        (and (sequencep projectile-known-projects)
             (copy-sequence projectile-known-projects))))

;; load the known projects
(projectile-load-known-projects)

(defun projectile-save-known-projects ()
  "Save PROJECTILE-KNOWN-PROJECTS to PROJECTILE-KNOWN-PROJECTS-FILE."
  (projectile-serialize projectile-known-projects
                        projectile-known-projects-file)
  (setq projectile-known-projects-on-file
        (and (sequencep projectile-known-projects)
             (copy-sequence projectile-known-projects))))

(defun projectile-merge-known-projects ()
  "Merge any change from `projectile-known-projects-file' and save to disk.

This enables multiple Emacs processes to make changes without
overwriting each other's changes."
  (let* ((known-now projectile-known-projects)
         (known-on-last-sync projectile-known-projects-on-file)
         (known-on-file
          (projectile-unserialize projectile-known-projects-file))
         (removed-after-sync (projectile-difference known-on-last-sync known-now))
         (removed-in-other-process
          (projectile-difference known-on-last-sync known-on-file))
         (result (delete-dups
                  (projectile-difference
                   (append known-now known-on-file)
                   (append removed-after-sync removed-in-other-process)))))
    (setq projectile-known-projects result)
    (projectile-save-known-projects)))

(define-ibuffer-filter projectile-files
    "Show Ibuffer with all buffers in the current project."
  (:reader (read-directory-name "Project root: " (ignore-errors (projectile-project-root)))
           :description nil)
  (with-current-buffer buf
    (equal (file-name-as-directory (expand-file-name qualifier))
           (ignore-errors (projectile-project-root)))))

(defun projectile-ibuffer-by-project (project-root)
  "Open an IBuffer window showing all buffers in PROJECT-ROOT."
  (let ((project-name (funcall projectile-project-name-function project-root)))
    (ibuffer nil (format "*%s Buffers*" project-name)
             (list (cons 'projectile-files project-root)))))

;;;###autoload
(defun projectile-ibuffer (prefix)
  "Open an IBuffer window showing all buffers in the current project.

Let user choose another project when PREFIX is supplied."
  (interactive "p")
  (let ((project-root (if (= prefix 4)
                          (projectile-completing-read
                           "Project name: "
                           (projectile-relevant-known-projects))
                        (projectile-project-root))))

    (projectile-ibuffer-by-project project-root)))

;;;; projectile-commander

(defconst projectile-commander-help-buffer "*Commander Help*")

(defvar projectile-commander-methods nil
  "List of file-selection methods for the `projectile-commander' command.
Each element is a list (KEY DESCRIPTION FUNCTION).
DESCRIPTION is a one-line description of what the key selects.")

;;;###autoload
(defun projectile-commander ()
  "Execute a Projectile command with a single letter.
The user is prompted for a single character indicating the action to invoke.
The `?' character describes then
available actions.

See `def-projectile-commander-method' for defining new methods."
  (interactive)
  (let* ((choices (mapcar #'car projectile-commander-methods))
         (prompt (concat "Commander [" choices "]: "))
         (ch (read-char-choice prompt choices))
         (fn (nth 2 (assq ch projectile-commander-methods))))
    (funcall fn)))

(defmacro def-projectile-commander-method (key description &rest body)
  "Define a new `projectile-commander' method.

KEY is the key the user will enter to choose this method.

DESCRIPTION is a one-line sentence describing how the method.

BODY is a series of forms which are evaluated when the find
is chosen."
  (let ((method `(lambda ()
                   ,@body)))
    `(setq projectile-commander-methods
           (cl-sort (copy-sequence
                     (cons (list ,key ,description ,method)
                           (assq-delete-all ,key projectile-commander-methods)))
                    (lambda (a b) (< (car a) (car b)))))))

(def-projectile-commander-method ?? "Commander help buffer."
  (ignore-errors (kill-buffer projectile-commander-help-buffer))
  (with-current-buffer (get-buffer-create projectile-commander-help-buffer)
    (insert "Projectile Commander Methods:\n\n")
    (dolist (met projectile-commander-methods)
      (insert (format "%c:\t%s\n" (car met) (cadr met))))
    (goto-char (point-min))
    (help-mode)
    (display-buffer (current-buffer) t))
  (projectile-commander))

(defun projectile-commander-bindings ()
  (def-projectile-commander-method ?A
    "Find ag on project."
    (call-interactively 'projectile-ag))

  (def-projectile-commander-method ?f
    "Find file in project."
    (projectile-find-file))

  (def-projectile-commander-method ?T
    "Find test file in project."
    (projectile-find-test-file))

  (def-projectile-commander-method ?b
    "Switch to project buffer."
    (projectile-switch-to-buffer))

  (def-projectile-commander-method ?d
    "Find directory in project."
    (projectile-find-dir))

  (def-projectile-commander-method ?D
    "Open project root in dired."
    (projectile-dired))

  (def-projectile-commander-method ?v
    "Open project root in vc-dir or magit."
    (projectile-vc))

  (def-projectile-commander-method ?V
    "Browse dirty projects"
    (projectile-browse-dirty-projects))

  (def-projectile-commander-method ?r
    "Replace a string in the project."
    (projectile-replace))

  (def-projectile-commander-method ?R
    "Regenerate the project's [e|g]tags."
    (projectile-regenerate-tags))

  (def-projectile-commander-method ?g
    "Run grep on project."
    (projectile-grep))

  (def-projectile-commander-method ?s
    "Switch project."
    (projectile-switch-project))

  (def-projectile-commander-method ?o
    "Run multi-occur on project buffers."
    (projectile-multi-occur))

  (def-projectile-commander-method ?j
    "Find tag in project."
    (projectile-find-tag))

  (def-projectile-commander-method ?k
    "Kill all project buffers."
    (projectile-kill-buffers))

  (def-projectile-commander-method ?e
    "Find recently visited file in project."
    (projectile-recentf)))

(projectile-commander-bindings)

(defun projectile-read-variable ()
  "Prompt for a variable and return its name."
  (completing-read "Variable: "
                   obarray
                   '(lambda (v)
                      (and (boundp v) (not (keywordp v))))
                   t))

(define-skeleton projectile-skel-variable-cons
  "Insert a variable-name and a value in a cons-cell."
  "Value: "
  "("
  (projectile-read-variable)
  " . "
  str
  ")")

(define-skeleton projectile-skel-dir-locals
  "Insert a .dir-locals.el template."
  nil
  "((nil . ("
  ("" '(projectile-skel-variable-cons) \n)
  resume:
  ")))")

;;;###autoload
(defun projectile-edit-dir-locals ()
  "Edit or create a .dir-locals.el file of the project."
  (interactive)
  (let ((file (expand-file-name ".dir-locals.el" (projectile-project-root))))
    (find-file file)
    (when (not (file-exists-p file))
      (unwind-protect
          (projectile-skel-dir-locals)
        (save-buffer)))))

;;; Minor mode
(defvar projectile-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "4 a") #'projectile-find-other-file-other-window)
    (define-key map (kbd "4 b") #'projectile-switch-to-buffer-other-window)
    (define-key map (kbd "4 C-o") #'projectile-display-buffer)
    (define-key map (kbd "4 d") #'projectile-find-dir-other-window)
    (define-key map (kbd "4 D") #'projectile-dired-other-window)
    (define-key map (kbd "4 f") #'projectile-find-file-other-window)
    (define-key map (kbd "4 g") #'projectile-find-file-dwim-other-window)
    (define-key map (kbd "4 t") #'projectile-find-implementation-or-test-other-window)
    (define-key map (kbd "5 a") #'projectile-find-other-file-other-frame)
    (define-key map (kbd "5 b") #'projectile-switch-to-buffer-other-frame)
    (define-key map (kbd "5 d") #'projectile-find-dir-other-frame)
    (define-key map (kbd "5 D") #'projectile-dired-other-frame)
    (define-key map (kbd "5 f") #'projectile-find-file-other-frame)
    (define-key map (kbd "5 g") #'projectile-find-file-dwim-other-frame)
    (define-key map (kbd "5 t") #'projectile-find-implementation-or-test-other-frame)
    (define-key map (kbd "!") #'projectile-run-shell-command-in-root)
    (define-key map (kbd "&") #'projectile-run-async-shell-command-in-root)
    (define-key map (kbd "a") #'projectile-find-other-file)
    (define-key map (kbd "b") #'projectile-switch-to-buffer)
    (define-key map (kbd "c") #'projectile-compile-project)
    (define-key map (kbd "d") #'projectile-find-dir)
    (define-key map (kbd "D") #'projectile-dired)
    (define-key map (kbd "e") #'projectile-recentf)
    (define-key map (kbd "E") #'projectile-edit-dir-locals)
    (define-key map (kbd "f") #'projectile-find-file)
    (define-key map (kbd "g") #'projectile-find-file-dwim)
    (define-key map (kbd "F") #'projectile-find-file-in-known-projects)
    (define-key map (kbd "i") #'projectile-invalidate-cache)
    (define-key map (kbd "I") #'projectile-ibuffer)
    (define-key map (kbd "j") #'projectile-find-tag)
    (define-key map (kbd "k") #'projectile-kill-buffers)
    (define-key map (kbd "l") #'projectile-find-file-in-directory)
    (define-key map (kbd "m") #'projectile-commander)
    (define-key map (kbd "o") #'projectile-multi-occur)
    (define-key map (kbd "p") #'projectile-switch-project)
    (define-key map (kbd "q") #'projectile-switch-open-project)
    (define-key map (kbd "P") #'projectile-test-project)
    (define-key map (kbd "r") #'projectile-replace)
    (define-key map (kbd "R") #'projectile-regenerate-tags)
    (define-key map (kbd "s g") #'projectile-grep)
    (define-key map (kbd "s s") #'projectile-ag)
    (define-key map (kbd "S") #'projectile-save-project-buffers)
    (define-key map (kbd "t") #'projectile-toggle-between-implementation-and-test)
    (define-key map (kbd "T") #'projectile-find-test-file)
    (define-key map (kbd "u") #'projectile-run-project)
    (define-key map (kbd "v") #'projectile-vc)
    (define-key map (kbd "V") #'projectile-browse-dirty-projects)
    (define-key map (kbd "x e") #'projectile-run-eshell)
    (define-key map (kbd "x t") #'projectile-run-term)
    (define-key map (kbd "x s") #'projectile-run-shell)
    (define-key map (kbd "z") #'projectile-cache-current-file)
    (define-key map (kbd "ESC") #'projectile-project-buffers-other-buffer)
    map)
  "Keymap for Projectile commands after `projectile-keymap-prefix'.")
(fset 'projectile-command-map projectile-command-map)

(defvar projectile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map projectile-keymap-prefix 'projectile-command-map)
    map)
  "Keymap for Projectile mode.")

(easy-menu-change
 '("Tools") "Projectile"
 '(["Find file" projectile-find-file]
   ["Find file in known projects" projectile-find-file-in-known-projects]
   ["Find test file" projectile-find-test-file]
   ["Find directory" projectile-find-dir]
   ["Find file in directory" projectile-find-file-in-directory]
   ["Find other file" projectile-find-other-file]
   ["Switch to buffer" projectile-switch-to-buffer]
   ["Jump between implementation file and test file" projectile-toggle-between-implementation-and-test]
   ["Kill project buffers" projectile-kill-buffers]
   ["Recent files" projectile-recentf]
   ["Edit .dir-locals.el" projectile-edit-dir-locals]
   "--"
   ["Open project in dired" projectile-dired]
   ["Switch to project" projectile-switch-project]
   ["Switch to open project" projectile-switch-open-project]
   ["Discover projects in directory" projectile-discover-projects-in-directory]
   ["Search in project (grep)" projectile-grep]
   ["Search in project (ag)" projectile-ag]
   ["Replace in project" projectile-replace]
   ["Multi-occur in project" projectile-multi-occur]
   ["Browse dirty projects" projectile-browse-dirty-projects]
   "--"
   ["Run shell" projectile-run-shell]
   ["Run eshell" projectile-run-eshell]
   ["Run term" projectile-run-term]
   "--"
   ["Cache current file" projectile-cache-current-file]
   ["Invalidate cache" projectile-invalidate-cache]
   ["Regenerate [e|g]tags" projectile-regenerate-tags]
   "--"
   ["Compile project" projectile-compile-project]
   ["Test project" projectile-test-project]
   ["Run project" projectile-run-project]
   "--"
   ["Project info" projectile-project-info]
   ["About" projectile-version])
 "Search Files (Grep)...")

(easy-menu-change '("Tools") "--" nil "Search Files (Grep)...")

;;;###autoload
(defcustom projectile-mode-line
  '(:eval (format " Projectile[%s(%s)]"
                  (projectile-project-name)
                  (projectile-project-type)))
  "Mode line lighter for Projectile.

The value of this variable is a mode line template as in
`mode-line-format'.  See Info Node `(elisp)Mode Line Format' for
details about mode line templates.

Customize this variable to change how Projectile displays its
status in the mode line.  The default value displays the project
name and type.  Set this variable to nil to disable the mode line
entirely."
  :group 'projectile
  :type 'sexp
  :risky t
  :package-version '(projectile . "0.12.0"))

(defun projectile-find-file-hook-function ()
  "Called by `find-file-hook' when `projectile-mode' is on.

The function does pretty much nothing when triggered on remote files
as all the operations it normally performs are extremely slow over
tramp."
  (unless (file-remote-p default-directory)
    (projectile-cache-files-find-file-hook)
    (projectile-track-known-projects-find-file-hook)
    (projectile-visit-project-tags-table)))

;;;###autoload
(define-minor-mode projectile-mode
  "Minor mode to assist project management and navigation.

When called interactively, toggle `projectile-mode'.  With prefix
ARG, enable `projectile-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `projectile-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `projectile-mode'.
Otherwise behave as if called interactively.

\\{projectile-mode-map}"
  :lighter projectile-mode-line
  :keymap projectile-mode-map
  :group 'projectile
  :require 'projectile
  :global t
  (cond
   (projectile-mode
    ;; initialize the projects cache if needed
    (unless projectile-projects-cache
      (setq projectile-projects-cache
            (or (projectile-unserialize projectile-cache-file)
                (make-hash-table :test 'equal))))
    (add-hook 'find-file-hook 'projectile-find-file-hook-function)
    (add-hook 'projectile-find-dir-hook #'projectile-track-known-projects-find-file-hook t)
    (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t t)
    (ad-activate 'compilation-find-file)
    (ad-activate 'delete-file))
   (t
    (remove-hook 'find-file-hook #'projectile-find-file-hook-function)
    (remove-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook t)
    (ad-deactivate 'compilation-find-file)
    (ad-deactivate 'delete-file))))

;;;###autoload
(define-obsolete-function-alias 'projectile-global-mode 'projectile-mode)

(provide 'projectile)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; projectile.el ends here
