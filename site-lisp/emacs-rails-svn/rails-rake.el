;;; rails-rake.el --- emacs-rails integraions with rake tasks.

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-scripts.el $
;; $Id: rails-scripts.el 117 2007-03-25 23:37:37Z dimaexe $

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(eval-when-compile
  (require 'rails-scripts))

(defvar rails-rake:history (list))

(defvar rails-rake:tasks-regexp "^rake \\([^ ]*\\).*# \\(.*\\)"
  "Regexp to match tasks list in `rake --tasks` output.")

(defun rails-rake:create-tasks-cache (file-name)
  "Create a cache file from rake --tasks output."
  (let ((tasks (loop for str in (split-string (rails-cmd-proxy:shell-command-to-string "rake --tasks") "\n")
                     for task = (when (string-not-empty str)
                                  (string=~ rails-rake:tasks-regexp str $1))
                     when task collect task)))
    (write-string-to-file file-name (prin1-to-string tasks))
    tasks))

(defun rails-rake:list-of-tasks ()
  "Return all available tasks and create tasks cache file."
  (rails-project:in-root
   (let* ((cache-file (rails-core:file "tmp/.tasks-cache")))
     (if (file-exists-p cache-file)
         (read-from-file cache-file)
       (rails-rake:create-tasks-cache cache-file)))))

(defun rails-rake:list-of-tasks-without-tests ()
  "Return available tasks without test actions."
  (when-bind
   (tasks (rails-rake:list-of-tasks))
   (sort (delete* nil
                  (mapcar
                   #'(lambda (it) (if (string=~ "^test\\($\\|:\\)" it t) nil it))
                   (rails-rake:list-of-tasks))
                  :if 'null)
         'string<)))

(defun rails-rake:task (task &optional major-mode)
  "Run a Rake task in RAILS_ROOT with MAJOR-MODE."
  (interactive (rails-completing-read "What task run" (rails-rake:list-of-tasks-without-tests)
                                      'rails-rake:history nil))
  (when task
    (rails-script:run "rake" (list task) major-mode)))

(defun rails-rake:migrate (&optional version)
  "Run the db:migrate task"
  (interactive)
  (rails-rake:task
   (concat
    "db:migrate"
    (typecase version
      (integer (format " VERSION=%.3i" version))
      (string (format " VERSION=%s" version))))))

(defun rails-rake:migrate-to-version (version)
  "Run migrate with VERSION."
  (interactive (rails-completing-read "Version of migration"
                                      (rails-core:migration-versions t)
                                      nil
                                      t))
  (when version
    (rails-rake:migrate version)))

(defun rails-rake:migrate-to-prev-version ()
  "Migrate to a previous version."
  (interactive)
  (let ((versions (rails-core:migration-versions t)))
    (rails-rake:migrate
     (when (< 2  (length versions))
       (nth 1 versions)))))

(provide 'rails-rake)
