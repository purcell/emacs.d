;;; eproject-tasks.el --- eproject task runner

;; Copyright (C) 2012 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>
;; Keywords: eproject

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

;;; Code:

(eval-when-compile (require 'cl))
(require 'eproject)

(declare-function anything-other-buffer "anything")
(declare-function helm-other-buffer "helm")



;;; Task configuration

(defvar eproject-tasks-sources
  '(eproject-tasks-source-path
    eproject-tasks-source-compile
    eproject-tasks-source-metadata
    eproject-tasks-source-attribute)
  "A list of TASK-SOURCE.

TASK-SOURCE is a list starts with a string NAME and the rest is
a plist.  Currently, only one key `:generator' is allowed and
it is mandatory.  Value of `:generator' is a function returns
a (possibly empty) list of TASKs runnable on the current project.

Each TASK is a list starts with a string NAME and the rest
is a plist.  Allowed keys are:

`:call' : function
    Lisp function to run.
`:shell' : string
    Shell command to run.
`:cd' : string
    Change directory to here.  Default is `eproject-root'.
`:available' : function
    A function to check if the task is available.
`:confirm' : boolean
    Confirm before running task.

`:call' or `:shell' must be given.")

(defun eproject-tasks-available-p (task)
  (destructuring-bind (name &key cd available &allow-other-keys)
      task
    (or (null available)
        (let ((default-directory (or cd (eproject-root))))
          (funcall available)))))

(defun eproject-tasks-process-task-source (task-source)
  (destructuring-bind (name &key generator)
      (eval task-source)
    (loop for task in (funcall generator)
          when (eproject-tasks-available-p task)
          collect task)))

(defun eproject-tasks-get (&optional sources)
  "Get tasks runnable on the current project."
  (apply #'append (mapcar #'eproject-tasks-process-task-source
                          (or sources eproject-tasks-sources))))


;;; Predefined sources

(defvar eproject-tasks-source-attribute
  '("Attribute"
    :generator eproject-tasks-source-attribute-generate))

(defun eproject-tasks-source-attribute-generate ()
  (eproject-attribute :tasks))

(defvar eproject-tasks-source-metadata
  '("Metadata"
    :generator eproject-tasks-source-metadata-generate))

(defun eproject-tasks-source-metadata-generate ()
  (eproject-get-project-metadatum (eproject-type) :tasks))

(defvar eproject-tasks-source-compile
  '("Compile"
    :generator eproject-tasks-source-compile-generate))

(defun eproject-tasks-source-compile-generate ()
  (let ((make (concat (file-name-as-directory (eproject-root))
                      "Makefile")))
    (when (file-exists-p make)
      '(("compile" :call compile)))))

(defvar eproject-tasks-source-path
  '("Path specific"
    :generator eproject-tasks-source-path-generate))

(defvar eproject-tasks-source-path-alist nil
  "List of (REGEXP . TASKS) where REGEXP is a regular expression
to test against `eproject-root' and TASKS is a list of runnable
tasks.")

(defun eproject-tasks-source-path-generate ()
  "Task source defined by the variable `eproject-tasks-source-path'."
  (let ((root (eproject-root)))
    (when root
      (loop for (regexp . tasks) in eproject-tasks-source-path-alist
            when (string-match-p regexp root)
            return tasks))))


;;; Task runner

(defun eproject-tasks-run (task)
  "Run TASK.
Prompt pops up asking task to run when used as a command.
See `eproject-tasks-sources' for the definition of TASK."
  (interactive (list (eproject-tasks-read)))
  (destructuring-bind (name &key call shell confirm cd &allow-other-keys)
      task
    (message "Running %s..." name)
    (if (or (null confirm)
            (y-or-n-p (format "Really run task '%s'? " name)))
        (let ((default-directory (or cd (eproject-root))))
          (when call
            (if (commandp call)
                (call-interactively call)
              (funcall call)))
          (when shell (eproject-tasks--run-shell shell))
          (message "Running %s...done" name))
      (message "Running %s...CANCELED!" name))))

(defun eproject-tasks--run-shell (shell)
  (shell-command (if (string-match-p " &$" shell)
                     shell
                   (concat shell " &"))))

(defun eproject-tasks-read ()
  "Read task name from prompt and return a TASK object."
  (let* ((tasks (eproject-tasks-get))
         (names (mapcar #'car tasks))
         (default (car names)))
    (unless tasks
      (error "No task available."))
    (assoc (completing-read (format "Task to run (default %s): " default)
                           names nil t nil nil default)
           tasks)))


;;; Helm/anything interface

(defvar eproject-tasks--current-buffer nil
  "Original buffer in which helm/anything command is invoked.
Same as `helm-current-buffer' or `anything-current-buffer'")

(defun eproject-tasks-helm-candidates (sources)
  (with-current-buffer eproject-tasks--current-buffer
    (mapcar (lambda (task) (cons (car task) task))
            (eproject-tasks-get sources))))

(defun eproject-tasks-helm-sources ()
  "Return a list of helm/anything sources.
Each helm/anything source represents one task source in
`eproject-tasks-sources'."
  (loop for task-source in eproject-tasks-sources
        for name = (car (eval task-source))
        collect
        `((name . ,(format "eproject tasks (%s)" name))
          (candidates
           . (lambda ()
               (eproject-tasks-helm-candidates (list ',task-source))))
          (action . (("Run" . eproject-tasks-run))))))

(defun helm-eproject-tasks ()
  (interactive)
  (let ((eproject-tasks--current-buffer (current-buffer)))
    (helm-other-buffer (eproject-tasks-helm-sources)
                       "*helm eproject tasks*")))

(defun anything-eproject-tasks ()
  (interactive)
  (let ((eproject-tasks--current-buffer (current-buffer)))
    (anything-other-buffer (eproject-tasks-helm-sources)
                           "*anything eproject tasks*")))


(provide 'eproject-tasks)
;;; eproject-tasks.el ends here
