;;; elein.el --- running leiningen commands from emacs

;; Copyright (C) 2010, 2011 R.W van 't Veer

;; Author: R.W. van 't Veer
;; Created: 2 Aug 2010
;; Keywords: tools processes
;; Package-Version: 20120120.316
;; Version: 0.2.2
;; URL: https://github.com/remvee/elein

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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
;; Provides support for running leiningen commands like swank and test.

;;; Code:

(eval-when-compile (require 'cl))

(defgroup elein nil
  "running leiningen commands from emacs"
  :prefix "elein-"
  :group 'applications)

(defcustom elein-lein "lein"
  "Leiningen 'lein' command."
  :type 'string
  :group 'elein)

(defcustom elein-standalone-swank-command "~/.lein/bin/swank-clojure"
  "Leiningen 'swank-clojure' command for standalone execution."
  :type 'string
  :group 'elein)

(defcustom elein-swank-buffer-name "*elein-swank*"
  "Buffer name for swank process."
  :type 'string
  :group 'elein)

(defcustom elein-swank-port 4005
  "Swank port to listen."
  :type 'integer
  :group 'elein)

(defcustom elein-swank-host "127.0.0.1"
  "Swank address to listen."
  :type 'string
  :group 'elein)

(defcustom elein-swank-options ":encoding '\"utf-8\"'"
  "Swank options."
  :type 'string
  :group 'elein)

(defcustom elein-slime-net-coding-system 'utf-8-unix
  "Coding system used for slime network connections.
Should match any :encoding specified in `elein-swank-options'.
See also `slime-net-valid-coding-systems'."
  :type 'symbol
  :group 'elein)

(defun elein-project-root ()
  "Look for project.clj file to find project root."
  (locate-dominating-file default-directory "project.clj"))

(defmacro elein-in-project-root (body)
  "Wrap BODY to make `default-directory' the project root."
  (let ((dir (gensym)))
    `(let ((,dir (elein-project-root)))
       (if ,dir
         (let ((default-directory ,dir)) ,body)
         (error "No leiningen project root found")))))

(defvar elein-task-alist nil
  "Holds cached task list by directory name.  The car of the
  value is the mtime of the project.clj file and the cdr is the
  task list itself.")

(defun elein-project-clj-mtime ()
  "Get mtime from the project.clj in the current project."
  (nth 5 (elein-in-project-root
          (file-attributes "project.clj"))))

(defun elein-list-tasks ()
  "Collect tasks for current project."
  (let* ((root (elein-project-root))
         (cached (assoc root elein-task-alist)))
    (if (and cached (equal (elein-project-clj-mtime) (cadr cached)))
      (cddr cached)
      (let ((tasks (elein-in-project-root
                    (let ((output (shell-command-to-string (concat elein-lein " help")))
                          (result nil)
                          (offset 0))
                      (while (string-match "^  \\(.*\\)" output offset)
                        (setq result (cons (match-string 1 output) result))
                        (setq offset (match-end 0)))
                      (sort result (lambda (a b) (string< a b)))))))
        (setq elein-task-alist (cons (cons root (cons (elein-project-clj-mtime)
                                                      tasks))
                                     elein-task-alist))
        tasks))))

(defun elein-swank-command ()
  "Build lein swank command from customization variables."
  (format "%s swank %d %s %s"
          elein-lein
          elein-swank-port
          elein-swank-host
          elein-swank-options))

(defun elein-standalone-swank-command ()
  "Build projectless lein swank command."
  (unless (file-exists-p (expand-file-name elein-standalone-swank-command))
    (error "can not find %s; use 'lein install swank-clojure VERSION' to install it"
           elein-standalone-swank-command))
  (format "%s %d :host %s %s"
          (expand-file-name elein-standalone-swank-command)
          elein-swank-port
          elein-swank-host
          elein-swank-options))

(defun elein-burried-shell-command (command buffer)
  "Same as `shell-command' but run process asynchronously, do not
show output and burry the given BUFFER."
  (flet ((display-buffer (buffer-or-name &optional not-this-window frame) nil))
    (bury-buffer buffer)
    (shell-command (concat command "&") buffer)))

(defun elein-swank-process-filter (process output)
  "Swank process filter to launch `slime-connect' when process is ready."
  (with-current-buffer elein-swank-buffer-name (insert output))

  (when (string-match "Connection opened on \\(local\\|127.0.0.1\\) port +\\([0-9]+\\)" output)
    (slime-set-inferior-process
     (slime-connect "localhost" (string-to-number (match-string 2 output)) elein-slime-net-coding-system)
     process)
    (set-process-filter process nil)))

;;;###autoload
(defun elein-swank (&optional prefix)
  "Launch lein swank and connect slime to it.  Interactively, a
PREFIX means launch a standalone swank session without a
project."
  (interactive "P")
  (let ((buffer (get-buffer-create elein-swank-buffer-name)))
    (if prefix
      (elein-burried-shell-command (elein-standalone-swank-command) buffer)
      (elein-in-project-root
       (elein-burried-shell-command (elein-swank-command) buffer)))

    (set-process-filter (get-buffer-process buffer) #'elein-swank-process-filter)

    (message "Starting swank..")))

;;;###autoload
(defun elein-kill-swank ()
  "Kill swank process started by lein swank."
  (interactive)
  (let ((process (get-buffer-process "*elein-swank*")))
    (when process
      (ignore-errors (slime-quit-lisp))
      (let ((timeout 10))
        (while (and (> timeout 0)
                    (eql 'run (process-status process)))
          (sit-for 1)
          (decf timeout)))
      (ignore-errors (kill-buffer "*elein-swank*")))))

;;;###autoload
(defun elein-reswank ()
  "Kill current lisp, restart lein swank and connect slime to it."
  (interactive)
  (elein-kill-swank)
  (elein-swank))

;;;###autoload
(defun elein-run-cmd (args)
  "Run 'lein ARGS' using `compile' in the project root directory."
  (interactive "sArguments: ")
  (elein-in-project-root (compile (concat elein-lein " " args))))

;;;###autoload
(defun elein-run-task (task)
  "Run 'lein TASK' using `compile' in the project root directory."
  (interactive (list (completing-read "Task: " (elein-list-tasks))))
  (elein-run-cmd task))

(defmacro elein-defun-run-task (task)
  "Define shortcut function for `elein-run-task' with argument TASK."
  `(defun ,(intern (concat "elein-" task)) ()
     ,(concat "Run 'lein " task "' in the project root directory.")
     (interactive)
     (elein-run-task ,task)))

;; define interactive elein-TASK commands for common tasks
(dolist (task '(classpath
                clean
                compile
                deploy
                deps
                help
                install
                jar
                pom
                test
                uberjar
                version))
  (eval `(elein-defun-run-task ,(symbol-name task))))

(provide 'elein)

;;; elein.el ends here
