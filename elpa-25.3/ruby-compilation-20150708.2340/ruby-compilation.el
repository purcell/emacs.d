;;; ruby-compilation.el --- run a ruby process in a compilation buffer

;; Copyright (C) 2008 Eric Schulte
;; Copyright (C) 2009-2015 Steve Purcell

;; Author: Eric Schulte
;; Maintainer: Steve Purcell <steve@sanityinc.com>
;; URL: https://github.com/eschulte/rinari
;; Package-Version: 20150708.2340
;; Version: 0.17
;; Created: 2008-08-23
;; Keywords: test convenience
;; Package-Requires: ((inf-ruby "2.2.1"))

;;; License:

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

;; Allow for execution of ruby processes dumping the results into a
;; compilation buffer.  Useful for executing tests, or rake tasks
;; where the ability to jump to errors in source code is desirable.
;;
;; The functions you will probably want to use are
;;
;; ruby-compilation-run
;; ruby-compilation-rake
;; ruby-compilation-this-buffer (C-x t)
;; ruby-compilation-this-test (C-x T)
;;

;;; TODO:

;; Clean up function names so they use a common prefix.
;; "p" doesn't work at the end of the compilation buffer.

;;; Code:

(require 'ansi-color)
(require 'pcomplete)
(require 'compile)
(require 'inf-ruby)
(require 'which-func)

(defvar ruby-compilation-error-regexp
  "^\\([[:space:]]*\\|.*\\[\\|[^\*].*at \\)\\[?\\([^[:space:]]*\\):\\([[:digit:]]+\\)[]:)\n]?"
  "Regular expression to match errors in ruby process output.")

(defvar ruby-compilation-error-regexp-alist
  `((,ruby-compilation-error-regexp 2 3))
  "A version of `compilation-error-regexp-alist' for use in rails logs.
Should be used with `make-local-variable'.")

(defvar ruby-compilation-executable "ruby"
  "What bin to use to launch the tests.  Override if you use JRuby etc.")

(defvar ruby-compilation-executable-rake "rake"
  "What bin to use to launch rake.  Override if you use JRuby etc.")

(defvar ruby-compilation-test-name-flag "-n"
  "What flag to use to specify that you want to run a single test.")

(defvar ruby-compilation-clear-between t
  "Whether to clear the compilation output between runs.")

(defvar ruby-compilation-reuse-buffers t
  "Whether to re-use the same comint buffer for focussed tests.")


;;; Core plumbing

(defun ruby-compilation-filter ()
  "Filter function for compilation output."
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(defun ruby-compilation--kill-any-orphan-proc ()
  "Ensure any dangling buffer process is killed."
  (let ((orphan-proc (get-buffer-process (buffer-name))))
    (when orphan-proc
      (kill-process orphan-proc))))

(define-compilation-mode ruby-compilation-mode "RubyComp"
  "Ruby compilation mode."
  (progn
    (set (make-local-variable 'compilation-error-regexp-alist) ruby-compilation-error-regexp-alist)
    (add-hook 'compilation-filter-hook 'ruby-compilation-filter nil t)
    ;; Set any bound buffer name buffer-locally
    (set (make-local-variable 'kill-buffer-hook)
         'ruby-compilation--kill-any-orphan-proc)))

;; Low-level API entry point
(defun ruby-compilation-do (name cmdlist)
  "In a buffer identified by NAME, run CMDLIST in `ruby-compilation-mode'.
Returns the compilation buffer."
  (save-some-buffers (not compilation-ask-about-save)
                     (when (boundp 'compilation-save-buffers-predicate)
                       compilation-save-buffers-predicate))
  (let* ((this-dir default-directory))
    (with-current-buffer (get-buffer-create (concat "*" name "*"))
        (setq default-directory this-dir)
        (compilation-start
         (concat (car cmdlist) " "
                 (mapconcat 'shell-quote-argument (cdr cmdlist) " "))
         'ruby-compilation-mode
         (lambda (m) (buffer-name))))))

(defun ruby-compilation--skip-past-errors (line-incr)
  "Repeatedly move LINE-INCR lines forward until the current line is not an error."
  (while (string-match ruby-compilation-error-regexp (thing-at-point 'line))
    (forward-line line-incr)))

(defun ruby-compilation-previous-error-group ()
  "Jump to the start of the previous error group in the current compilation buffer."
  (interactive)
  (compilation-previous-error 1)
  (ruby-compilation--skip-past-errors -1)
  (forward-line 1)
  (recenter))

(defun ruby-compilation-next-error-group ()
  "Jump to the start of the previous error group in the current compilation buffer."
  (interactive)
  (ruby-compilation--skip-past-errors 1)
  (compilation-next-error 1)
  (recenter))

(defvar ruby-compilation-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q"    'quit-window)
    (define-key map "p"    'previous-error-no-select)
    (define-key map "n"    'next-error-no-select)
    (define-key map "\M-p" 'ruby-compilation-previous-error-group)
    (define-key map "\M-n" 'ruby-compilation-next-error-group)
    (define-key map (kbd "C-c C-c") 'comint-interrupt-subjob)
    map)
  "Key map for Ruby Compilation minor mode.")

(define-minor-mode ruby-compilation-minor-mode
  "Enable Ruby Compilation minor mode providing some key-bindings
  for navigating ruby compilation buffers."
  nil
  " ruby:comp"
  ruby-compilation-minor-mode-map
  (when ruby-compilation-clear-between
    (delete-region (point-min) (point-max))))

;; So we can invoke it easily.
;;;###autoload
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "C-x t") 'ruby-compilation-this-buffer)
     (define-key ruby-mode-map (kbd "C-x T") 'ruby-compilation-this-test)))

;; So we don't get warnings with .dir-settings.el files
(dolist (executable (list "jruby" "rbx" "ruby1.9" "ruby1.8" "ruby"))
  (add-to-list 'safe-local-variable-values
               (cons 'ruby-compilation-executable executable)))


;;; Basic public interface

;;;###autoload
(defun ruby-compilation-run (cmd &optional ruby-options name)
  "Run CMD using `ruby-compilation-executable' in a ruby compilation buffer.
Argument RUBY-OPTIONS can be used to specify additional
command line args for ruby.  If supplied, NAME will be used in
place of the script name to construct the name of the compilation
buffer."
  (interactive "FRuby Comand: ")
  (let ((name (or name (file-name-nondirectory (car (split-string cmd)))))
        (cmdlist (append (list ruby-compilation-executable)
                         ruby-options
                         (split-string (expand-file-name cmd)))))
    (pop-to-buffer (ruby-compilation-do name cmdlist))))

;;;###autoload
(defun ruby-compilation-this-buffer ()
  "Run the current buffer through Ruby compilation."
  (interactive)
  (ruby-compilation-run (buffer-file-name)))


;;; Special handling for rake and capistrano

(defun ruby-compilation-extract-output-matches (command pattern)
  "Run COMMAND, and return all the matching strings for PATTERN."
  (delq nil (mapcar #'(lambda(line)
                        (when (string-match pattern line)
                          (match-string 1 line)))
                  (split-string (shell-command-to-string command) "[\n]"))))

(defun ruby-compilation--format-env-vars (pairs)
  "Convert PAIRS of (name . value) into a list of name=value strings."
  (mapconcat (lambda (pair)
               (format "%s=%s" (car pair) (cdr pair)))
             pairs " "))

(defvar ruby-compilation-rake-tasks-cache nil
  "An alist with Rakefile directories as keys, and (MODTIME . (TASK-NAMES)) as values.")

(defun ruby-compilation-rake--clear-task-cache-for-dir (dir)
  "Remove any cached rake tasks for DIR."
  (setq ruby-compilation-rake-tasks-cache
        (delq (assoc dir ruby-compilation-rake-tasks-cache)
              ruby-compilation-rake-tasks-cache)))

(defun ruby-compilation--rakefile-dir ()
  "Return directory in which Rakefile is found, or nil if no such file exists."
  (locate-dominating-file default-directory "Rakefile"))

(defun pcmpl-rake-tasks ()
  "Return a list of all the rake tasks defined in the current projects."
  (let ((rakefile-dir (ruby-compilation--rakefile-dir)))
    (unless rakefile-dir
      (error "No Rakefile found"))
    (let ((cached (assoc rakefile-dir ruby-compilation-rake-tasks-cache))
          (rakefile-modtime (elt (file-attributes (expand-file-name "Rakefile" rakefile-dir)) 5)))
      (if (and cached (equal (cadr cached) rakefile-modtime))
          (cddr cached)
        (message "Building task completion list...")
        (let ((tasks (ruby-compilation-extract-output-matches "rake -T" "rake \\([^ ]+\\)")))
          (ruby-compilation-rake--clear-task-cache-for-dir rakefile-dir)
          (setq ruby-compilation-rake-tasks-cache
                (push (cons rakefile-dir (cons rakefile-modtime tasks))
                      ruby-compilation-rake-tasks-cache))
          tasks)))))

;;;###autoload
(defun pcomplete/rake ()
  "Start pcompletion using the list of available rake tasks."
  (pcomplete-here (pcmpl-rake-tasks)))

;;;###autoload
(defun ruby-compilation-rake-refresh-tasks ()
  "Reset the list of available rake tasks for the current Rakefile environment."
  (interactive)
  (ruby-compilation-rake--clear-task-cache-for-dir
   (ruby-compilation--rakefile-dir)))

;;;###autoload
(defun ruby-compilation-rake (&optional edit task env-vars)
  "Run a rake process dumping output to a ruby compilation buffer.
If EDIT is t, prompt the user to edit the command line.  If TASK
is not supplied, the user will be prompted.  ENV-VARS is an
optional list of (name . value) pairs which will be passed to rake.

The list of rake tasks will be remembered between invocations (on
a per-Rakefile basis) in the variable
`ruby-compilation-rake-tasks-cache'.  If the Rakefile is updated,
the available tasks will automatically be refreshed.  Use function
`ruby-compilation-rake-refresh-tasks' to force an update of the
available tasks, e.g. if tasks defined outside the Rakefile change."
  (interactive "P")
  (let ((rakefile-dir (ruby-compilation--rakefile-dir)))
    (unless rakefile-dir
      (error "No Rakefile found"))
    (let* ((default-directory rakefile-dir)
           (task (concat
                  (or task (if (stringp edit) edit)
                      (completing-read "Rake: " (pcmpl-rake-tasks)))
                  " "
                  (ruby-compilation--format-env-vars env-vars)))
           (rake-args (if (and edit (not (stringp edit)))
                          (read-from-minibuffer "Edit Rake Command: " (concat task " "))
                        task)))
      (pop-to-buffer (ruby-compilation-do
                      "rake" (cons ruby-compilation-executable-rake
                                   (split-string rake-args)))))))


(defun pcmpl-cap-tasks ()
   "Return a list of all the cap tasks defined in the current project."
   (ruby-compilation-extract-output-matches "cap -T" "cap \\([^ ]+\\)"))

;;;###autoload
(defun pcomplete/cap ()
  "Start pcompletion using the list of available capistrano tasks."
  (pcomplete-here (pcmpl-cap-tasks)))

;;;###autoload
(defun ruby-compilation-cap (&optional edit task env-vars)
  "Run a capistrano process dumping output to a ruby compilation buffer.
If EDIT is t, prompt the user to edit the command line.  If TASK
is not supplied, the user will be prompted.  ENV-VARS is an
optional list of (name . value) pairs which will be passed to
capistrano."
  (interactive "P")
  (let* ((task (concat
                (or task
                    (when (stringp edit) edit)
                    (completing-read "Cap: " (pcmpl-cap-tasks)))
                " "
                (ruby-compilation--format-env-vars env-vars)))
         (cap-args (if (and edit (not (stringp edit)))
                       (read-from-minibuffer "Edit Cap Command: " (concat task " "))
                     task)))
    (if (string-match "shell" task)
        (with-current-buffer (run-ruby (concat "cap " cap-args) "cap")
          (dolist (var '(inf-ruby-first-prompt-pattern inf-ruby-prompt-pattern))
            (set (make-local-variable var) "^cap> ")))
      (progn ;; handle all cap commands aside from shell
        (pop-to-buffer (ruby-compilation-do "cap" (cons "cap" (split-string cap-args))))
        (ruby-capistrano-minor-mode) ;; override some keybindings to make interaction possible
        (push (cons 'ruby-capistrano-minor-mode ruby-capistrano-minor-mode-map)
              minor-mode-map-alist)))))

(defvar ruby-capistrano-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" 'self-insert-command)
    (define-key map "p" 'self-insert-command)
    (define-key map "q" 'self-insert-command)
    (define-key map [return] 'comint-send-input) map)
  "Key map for Ruby Capistrano minor mode.")

(define-minor-mode ruby-capistrano-minor-mode
  "Enable Ruby Compilation minor mode providing some key-bindings
  for navigating ruby compilation buffers."
  nil
  " capstrano"
  ruby-capistrano-minor-mode-map)


;;; Running tests

(defun ruby-compilation-this-test-buffer-name (test-name)
  "The name of the buffer in which test-at-point will run TEST-NAME."
  (interactive)
  (if ruby-compilation-reuse-buffers
      (file-name-nondirectory (buffer-file-name))
    (format "ruby: %s - %s"
            (file-name-nondirectory (buffer-file-name))
            test-name)))

(defun ruby-compilation-this-test-name ()
  "Return the name of the test at point."
  (let ((this-test (which-function)))
    (when (listp this-test)
      (setq this-test (car this-test)))
    (if (or (not this-test)
            (not (string-match "#test_" this-test)))
        (message "Point is not in a test.")
      (cadr (split-string this-test "#")))))

;;;###autoload
(defun ruby-compilation-this-test ()
  "Run the test at point through Ruby compilation."
  (interactive)
  (let ((test-name (ruby-compilation-this-test-name)))
    (pop-to-buffer (ruby-compilation-do
                    (ruby-compilation-this-test-buffer-name test-name)
                    (list ruby-compilation-executable
                          (buffer-file-name)
                          ruby-compilation-test-name-flag test-name)))))


(provide 'ruby-compilation)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; ruby-compilation.el ends here
