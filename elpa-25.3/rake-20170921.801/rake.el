;;; rake.el --- Run rake commands

;; Copyright (C) 2014 Adam Sokolnicki

;; Author:            Adam Sokolnicki <adam.sokolnicki@gmail.com>
;; URL:               https://github.com/asok/rake.el
;; Package-Version: 20170921.801
;; Version:           0.3.3
;; Keywords:          rake, ruby
;; Package-Requires:  ((f "0.13.0") (dash "1.5.0") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

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
;;
;; Package to interact with rake command - make for Ruby.
;; It uses completion to choose a rake task to run.
;; It can use one of the ruby preloaders and caching to speed up the execution of rake.
;;
;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'compile)
(require 'dash)
(require 'f)

(defmacro rake--with-root (root body-form)
  `(let* ((default-directory ,root))
     ,body-form))

(defun rake--choose-command-prefix (root cases)
  (cond ((rake--spring-p root)
         (plist-get cases :spring))
        ((rake--zeus-p root)
         (plist-get cases :zeus))
        ((rake--bundler-p root)
         (plist-get cases :bundler))
        (t
         (plist-get cases :vanilla))))

(defcustom rake-enable-caching t
  "When t enables tasks caching."
  :group 'rake
  :type 'boolean)

(defcustom rake-cache-file
  (expand-file-name "rake.cache" user-emacs-directory)
  "The name of rake's cache file."
  :group 'rake
  :type 'string)

(defcustom rake-completion-system 'ido
  "The completion system to be used by rake."
  :group 'rake
  :type 'symbol
  :options '(ido grizzl helm default))

(defconst rake--edit-command 4)
(defconst rake--omit-cache   16)

(defun rake--spring-p (root)
  (let ((root (directory-file-name root)))
    (or
     ;; Older versions
     (file-exists-p (format "%s/tmp/spring/spring.pid" root))
     ;; 0.9.2+
     (file-exists-p (format "%s/spring/%s.pid" temporary-file-directory (md5 root)))
     ;; 1.2.0+
     (let* ((path (or (getenv "XDG_RUNTIME_DIR") temporary-file-directory))
            (ruby-version (shell-command-to-string "ruby -e 'print RUBY_VERSION'"))
            (application-id (md5 (concat ruby-version root))))
       (or
        (file-exists-p (format "%s/spring/%s.pid" path application-id))
        ;; 1.5.0+
        (file-exists-p (format "%s/spring-%s/%s.pid" path (user-real-uid) application-id)))))))

(defun rake--zeus-p (root)
  (file-exists-p (expand-file-name ".zeus.sock" root)))

(defun rake--bundler-p (root)
  (locate-dominating-file root "Gemfile"))

(defun rake--vertical-ido-on-p ()
  (and
   (boundp 'ido-vertical-decorations)
   (eq ido-decorations ido-vertical-decorations)))

(defun rake--vertical-completion-system-p ()
  (cl-case rake-completion-system
    ('grizzl t)
    ('helm t)
    ('ido (rake--vertical-ido-on-p))
    (t nil)))

(defun rake--root ()
  (file-truename (locate-dominating-file default-directory "Rakefile")))

(defun rake--deserialize-cache ()
  "Read data serialized by `rake--serialize-cache' from `rake-cache-file'."
  (when (file-exists-p rake-cache-file)
    (with-temp-buffer
      (insert-file-contents rake-cache-file)
      (read (buffer-string)))))

(defvar rake--cache
  (or (rake--deserialize-cache)
      (make-hash-table :test 'equal)))

(defvar rake--last-root nil)
(defvar rake--last-task nil)
(defvar rake--last-mode nil)

(defun rake--serialize-cache ()
  "Serialize `rake--cache' to `rake-cache-file'.
The saved data can be restored with `rake--deserialize-cache'."
  (when (file-writable-p rake-cache-file)
    (with-temp-file rake-cache-file
      (insert (let (print-length) (prin1-to-string rake--cache))))))

(defun rake--tasks-output (root)
  (shell-command-to-string
   (rake--choose-command-prefix root
                                (list :zeus "zeus rake -T -A"
                                      :spring "bundle exec spring rake -T -A"
                                      :bundler "bundle exec rake -T -A"
                                      :vanilla "rake -T -A"))))

(defun rake--parse-tasks (output)
  "Parses the OUTPUT of rake command with list of tasks. Returns a list of tasks."
  (--keep it
          (--map (if (string-match "rake \\(.+\\)$" it)
                     (match-string 1 it))
                 (split-string output "[\n]"))))

(defun rake--fresh-tasks (root)
  "Returns list of the rake tasks for the project in ROOT."
  (rake--parse-tasks (rake--tasks-output root)))

(defun rake--cached-tasks (arg root)
  "Returns cached list of the tasks for project in ROOT.
If ARG is 16 then regenerate the cache first.
If ARG is not 16 and the tasks are not found for the project it will regenerate the cache."
  (when (= arg rake--omit-cache)
    (rake--regenerate-cache root))
  (or (gethash root rake--cache) (rake--regenerate-cache root)))

(defun rake--regenerate-cache (root)
  "Regenerates cache for the tasks for the project in ROOT dir and saves it
to `rake-cache-file'. Returns a list of the tasks for the project."
  (let ((tasks (rake--fresh-tasks root)))
    (puthash root tasks rake--cache)
    (rake--serialize-cache)
    tasks))

(defun rake--cached-or-fresh-tasks (arg root)
  "Returns a list of all the rake tasks defined for the project in ROOT.
If `rake-enable-caching' is t look in the cache, if not fallback to calling rake."
  (if rake-enable-caching
      (rake--cached-tasks arg root)
    (rake--fresh-tasks root)))

(defun rake--tasks-without-doscstrings (tasks)
  (--map (rake--trim-docstring it) tasks))

(defun rake--trim-docstring (task)
  (replace-regexp-in-string "[ ]*#.*$" "" task))

(defun rake--completing-read (prompt choices)
  (cl-case rake-completion-system
    ('ido     (ido-completing-read prompt choices))
    ('default (completing-read     prompt choices))
    ('helm (if (fboundp 'helm-comp-read)
               (helm-comp-read prompt choices
                               :candidates-in-buffer t
                               :must-match 'confirm)
             (user-error "Please install helm first")))
    ('grizzl (if (and (fboundp 'grizzl-completing-read) (fboundp 'grizzl-make-index))
                 (grizzl-completing-read prompt (grizzl-make-index choices))
               (user-error "Please install grizzl first")))
    (t (funcall rake-completion-system prompt choices))))

(defun rake--read-task (root arg)
  (let ((tasks (rake--cached-or-fresh-tasks arg root)))
    (rake--trim-docstring
     (rake--completing-read "Rake task: "
                            (if (rake--vertical-completion-system-p)
                                tasks
                              (rake--tasks-without-doscstrings tasks))))))

(defun rake--apply-ansi-color ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))

(defun rake--compile (root task mode)
  (setq rake--last-root root
        rake--last-task task
        rake--last-mode mode)
  (rake--with-root root (compile task mode)))

;;;###autoload
(defun rake-compile (task-name &optional mode)
  "Runs TASK-NAME from the directory returned by `rake--root'.
The optional MODE can be passed to specify
which mode the compilation buffer should run in."
  (let* ((root (rake--root))
         (prefix (rake--choose-command-prefix root
                                              (list :spring  "bundle exec spring rake "
                                                    :zeus    "zeus rake "
                                                    :bundler "bundle exec rake "
                                                    :vanilla "rake "))))
    (rake--compile root (concat prefix task-name) (or mode 'rake-compilation-mode))))

;;;###autoload
(defun rake-rerun ()
  "Re-runs the last task"
  (interactive)
  (when (not rake--last-root)
    (error "No task was run"))
  (rake--compile rake--last-root rake--last-task rake--last-mode))

(define-derived-mode rake-compilation-mode compilation-mode "Rake Compilation"
  "Compilation mode used by `rake' command."
  (add-hook 'compilation-filter-hook 'rake--apply-ansi-color nil t))

;;;###autoload
(defun rake-regenerate-cache ()
  "Regenerates the rake's cache for the current project."
  (interactive)
  (rake--regenerate-cache (rake--root)))

;;;###autoload
(defun rake-find-task (arg)
  "Finds a rake task."
  (interactive "P")
  (let* ((root (or (rake--root) (user-error "Rakefile not found")))
         (arg (or (car arg) 0))
         (prefix (rake--choose-command-prefix root
                                              (list :spring  "bundle exec spring rake --where "
                                                    :zeus    "zeus rake --where "
                                                    :bundler "bundle exec rake --where "
                                                    :vanilla "rake --where ")))
         (task (rake--read-task root arg))
         (output (shell-command-to-string (concat prefix task))))
    (when (string-match "^rake [a-zA-Z:]+[ ]+\\([^ ]+\\):\\([0-9]+\\):" output)
      ;; find-file will alter the match data so we introduce let here
      (let ((file-name (match-string 1 output))
            (line-num (string-to-number (match-string 2 output))))
        (find-file file-name)
        (goto-line line-num)))))

;;;###autoload
(defun rake (arg &optional compilation-mode)
  "Runs rake command."
  (interactive "P")
  (let* ((root (or (rake--root) (user-error "Rakefile not found")))
         (arg (or (car arg) 0))
         (prefix (rake--choose-command-prefix root
                                              (list :spring  "bundle exec spring rake "
                                                    :zeus    "zeus rake "
                                                    :bundler "bundle exec rake "
                                                    :vanilla "rake ")))
         (task (rake--read-task root arg))
         (task (if (= arg rake--edit-command)
                   (read-string "Rake command: " (concat prefix task " "))
                 (concat prefix task)))
         (mode (or compilation-mode 'rake-compilation-mode)))
    (rake--compile root task mode)))

(provide 'rake)

;;; rake.el ends here
