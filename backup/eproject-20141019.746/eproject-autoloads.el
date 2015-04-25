;;; eproject-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "eproject-anything" "eproject-anything.el"
;;;;;;  (21817 51935 24312 994000))
;;; Generated autoloads from eproject-anything.el

(defvar anything-c-source-eproject-files '((name . "Files in eProject") (init lambda nil (if (buffer-file-name) (setq anything-eproject-root-dir (eproject-maybe-turn-on)) (setq anything-eproject-root-dir 'nil))) (candidates lambda nil (if anything-eproject-root-dir (eproject-list-project-files anything-eproject-root-dir))) (type . file)) "\
Search for files in the current eProject.")

(defvar anything-c-source-eproject-buffers '((name . "Buffers in this eProject") (init lambda nil (if (buffer-file-name) (setq anything-eproject-root-dir (eproject-maybe-turn-on)) (setq anything-eproject-root-dir 'nil))) (candidates lambda nil (if anything-eproject-root-dir (mapcar 'buffer-name (cdr (assoc anything-eproject-root-dir (eproject--project-buffers)))))) (volatile) (type . buffer)) "\
Search for buffers in this project.")

(autoload 'anything-eproject-files "eproject-anything" "\
Preconfigured `anything' for searching files inside current eproject.

\(fn)" t nil)

(autoload 'anything-eproject-buffers "eproject-anything" "\
Preconfigured `anything' for opening buffers. Searches for
buffers in the current project, then other buffers, also gives
option of recentf. Replaces switch-to-buffer.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "eproject-compile" "eproject-compile.el" (21817
;;;;;;  51934 974312 992000))
;;; Generated autoloads from eproject-compile.el

(autoload 'eproject-compile "eproject-compile" "\
Run `compile' in the project root.

This uses a computed history based on project attributes, the
existing `compile-history', and `compile-command' which may have
been locally set by a mode.

To provide defaults for a project or project type, set the
`:common-compiles' attribute to a list of strings representing
the command to invoke.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "eproject-extras" "eproject-extras.el" (21817
;;;;;;  51934 924312 991000))
;;; Generated autoloads from eproject-extras.el

(autoload 'eproject-find-file "eproject-extras" "\
Present the user with a list of files in the current project.
to select from, open file when selected.

\(fn)" t nil)

(autoload 'eproject--project-buffers "eproject-extras" "\
Return an alist mapping each project root to its open buffers.

Does not list the project if it doesn't have any buffers.

\(fn)" nil nil)

(autoload 'eproject-switch-to-buffer "eproject-extras" "\
Interactively switch to a buffer belonging to the same project.

With prefix argument 4, first ask which project.

\(fn &optional PREFIX)" t nil)

(autoload 'eproject-switch-to-buffer-other-window "eproject-extras" "\
Interactively switch the other window to a buffer belonging to the same project.

With prefix argument 4, first ask which project.

\(fn &optional PREFIX)" t nil)

(autoload 'eproject-switch-to-buffer-other-frame "eproject-extras" "\
Interactively switch the other frame to a buffer belonging to the same project.

With prefix argument 4, first ask which project.

\(fn &optional PREFIX)" t nil)

(autoload 'eproject-ibuffer "eproject-extras" "\
Open an IBuffer window showing all buffers in the current project, or named project if PREFIX arg is supplied.

\(fn PREFIX)" t nil)

(autoload 'eproject-kill-project-buffers "eproject-extras" "\
Kill every buffer in the current project, including the current buffer.

If PREFIX is specified, prompt for a project name and kill those
buffers instead.

\(fn PREFIX)" t nil)

(autoload 'eproject-revisit-project "eproject-extras" "\
Given a project name, visit the root directory.

If PREFIX arg is supplied, run `eproject-find-file'.

\(fn PREFIX)" t nil)

(autoload 'eproject-grep "eproject-extras" "\
Search all files in the current project for REGEXP.

\(fn REGEXP)" t nil)

(autoload 'eproject-todo "eproject-extras" "\
Display a project TODO list.

Customize `eproject-todo-expressions' to control what this function looks for.

\(fn)" t nil)

(autoload 'eproject-multi-isearch-buffers "eproject-extras" "\
Do a `multi-isearch' on opened buffers in the current project.

Run `eproject-open-all-project-files' first or just
`eproject-grep' if you want to search all project files.

\(fn)" t nil)

(autoload 'eproject-eshell-cd-here "eproject-extras" "\
If there is an EShell buffer, cd to the project root in that buffer.

With the prefix arg LOOK-IN-INVISIBLE-BUFFERS looks in buffers that are not currently displayed.

\(fn &optional LOOK-IN-INVISIBLE-BUFFERS)" t nil)

(autoload 'eproject-current-working-directory "eproject-extras" "\
Return the project root directory for most recently visited
buffer.  Fallback to the directory of the buffer when it is
not in a project.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("eproject-android.el" "eproject-arduino.el"
;;;;;;  "eproject-clojure-leiningen.el" "eproject-javascript-bower.el"
;;;;;;  "eproject-javascript-grunt.el" "eproject-javascript-npm.el"
;;;;;;  "eproject-perl.el" "eproject-php-composer.el" "eproject-pkg.el"
;;;;;;  "eproject-python.el" "eproject-ruby-on-rails.el" "eproject-ruby.el"
;;;;;;  "eproject-tags.el" "eproject-tasks.el" "eproject.el" "helm-eproject.el")
;;;;;;  (21817 51935 607930 211000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; eproject-autoloads.el ends here
