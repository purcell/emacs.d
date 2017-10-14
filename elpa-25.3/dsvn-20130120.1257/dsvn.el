;;; dsvn.el --- Subversion interface

;; Copyright 2006-2010 Virtutech AB
;; Copyright 2010 Intel

;; Author: David Kågedal <davidk@lysator.liu.se>
;;	   Mattias Engdegård <mattiase@acm.org>
;; Maintainer: Mattias Engdegård <mattiase@acm.org>
;; Created: 27 Jan 2006
;; Keywords: docs
;; Package-Version: 20130120.1257

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; Commentary:
;; 
;; This is an interface for managing Subversion working copies.  It
;; can show you an up-to-date view of the current status, and commit
;; changes. If also helps you do other tasks such as updating,
;; switching, diffing and more.
;;
;; To get you started, add this line to your startup file:
;;
;;  (autoload 'svn-status "dsvn" "Run `svn status'." t)
;;  (autoload 'svn-update "dsvn" "Run `svn update'." t)
;;
;; This file integrates well with vc-svn, so you might want to do this
;; as well:
;;
;;   (require 'vc-svn)
;;
;; To get the status view, type
;;
;;   M-x svn-status
;;
;; and select a directory where you have a checked-out Subversion
;; working copy.  A buffer will be created that shows what files you
;; have modified, and any unknown files.  The file list corresponds
;; closely to that produced by "svn status", only slightly
;; reformatted.
;;
;; Navigate through the file list using "n" and "p", for next and
;; previous file, respectively.
;;
;; You can get a summary of available commands by typing "?".
;;
;; Some commands operate on files, and can either operate on the file
;; under point, or on a group of files that have been marked.  The
;; commands used for marking a file are the following:
;;
;;   m      mark and go down  
;;   DEL    unmark and go up  
;;   u      unmark and go down
;;   SPC    toggle mark       
;;   M-DEL  unmark all       
;;
;; The commands that operate on files are:
;;
;;   f      Visit the file under point (does not use marks)
;;   o      Visit the file under point in another window (does not use marks)
;;   =      Show diff of uncommitted changes.  This does not use marks
;;            unless you give a prefix argument (C-u) 
;;   c      Commit files
;;   a      Add files
;;   r      Remove files
;;   R      Resolve conflicts
;;   M      Rename/move files
;;   U      Revert files
;;   P      View or edit properties of the file or directory under point
;;            (does not use marks)
;;   l      Show log of file or directory under point (does not use marks)
;;
;; These commands update what is shown in the status buffer:
;;
;;   g      Rerun "svn status" to update the list.  Use a prefix
;;            argument (C-u) to clear the list first to make sure that
;;            it is correct.
;;   s      Update status of selected files
;;   S      Show status of specific file or directory
;;   x      Expunge unchanged files from the list
;;
;; To update the working copy:
;;
;;   M-u    Run "svn update".  If a prefix argument is given (C-u),
;;            you will be prompted for a revision to update to.
;;   M-s    Switch working copy to another branch.
;;   M-m    Merge in changes using "svn merge".
;;
;; To view the Subversion log, type "M-x svn-log".
;;
;; Bugs and missing features:
;; 
;; - Annotate (blame).
;; - Log, with a useful log mode where the user can easily view any revision
;;   as a diff or visit a revision of a file in a buffer.
;; - Integration with ediff or similar to resolve conflicts.

(require 'vc)
(require 'log-edit)
(require 'uniquify)

(defconst svn-status-msg-col   1)
(defconst svn-status-flags-col 11)
(defconst svn-status-mark-col  18)
(defconst svn-status-file-col  20)

(defgroup dsvn nil
  "Settings for dsvn."
  :group 'tools)

(defcustom svn-program "svn"
  "*The svn program to run"
  :type 'string
  :group 'dsvn)  

(defcustom svn-restore-windows nil
  "*Non-nil means that the window configuration is restored after commit"
  :type 'boolean
  :group 'dsvn)

(defcustom svn-diff-args '("-x" "-p")
  "*Additional arguments used for all invocations of `svn diff'."
  :type '(repeat string)
  :group 'dsvn)

;; start-file-process and process-file are needed for tramp but only appeared
;; in Emacs 23 and 22 respectively.
(setq svn-start-file-process
  (if (fboundp 'start-file-process) 'start-file-process 'start-process))
(setq svn-process-file
  (if (fboundp 'process-file) 'process-file 'call-process))

;; Run svn with default (US-English) messages, since we are going to
;; parse them.
(setq svn-process-environment '("LC_MESSAGES=C"))

(defun svn-call-in-svn-environment (func)
  ;; Dynamic rebinding of process-environment
  (let ((process-environment
         (append svn-process-environment process-environment)))
    (funcall func)))

(defun svn-start-svn-process (buffer args)
  "Start an svn process associated to BUFFER, with command-line
arguments ARGS. Return the process object for it."
  (svn-call-in-svn-environment
   (lambda ()
     (apply svn-start-file-process "svn" buffer svn-program args))))

(defun svn-call-svn (infile buffer display args)
  "Call svn synchronously. Arguments are like process-file."
  (svn-call-in-svn-environment
   (lambda ()
     (apply svn-process-file svn-program infile buffer display args))))

(defun svn-call-process (buffer args)
  "Run svn and wait for it to finish.
Argument BUFFER is the buffer in which to insert output.
Optional argument ARGS are the arguments to svn."
  (let ((proc (svn-start-svn-process buffer args)))
    (set-process-coding-system proc 'utf-8)
    (set-process-filter proc 'svn-output-filter)
    (while (eq (process-status proc) 'run)
      (accept-process-output proc 5)
      (sit-for 0))))

(defun svn-run-with-output (subcommand &optional args mode)
  "Run 'svn' with output to another window.
Argument SUBCOMMAND is the command to execute.
Optional argument ARGS is a list of the arguments to the command.
Optional argument MODE is the major mode to use for the output buffer.

Return non-NIL if there was any output."
  (let ((buf (get-buffer-create "*svn output*"))
        (dir default-directory)
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (if mode
          (funcall mode)
        (fundamental-mode))
      (setq default-directory dir)
      (setq buffer-read-only t)
      (let ((proc (svn-start-svn-process buf (cons subcommand args))))
        (set-process-coding-system proc 'utf-8)
        (set-process-filter proc 'svn-output-filter)
        (while (eq (process-status proc) 'run)
          (accept-process-output proc 5)
          (sit-for 0)))
      (if (= (point-min) (point-max))
          nil
        (save-selected-window
          (select-window (display-buffer buf))
          (goto-char (point-min)))
        t))))

(defun svn-run-hidden (command args)
  "Run 'svn' without showing output.
Argument COMMAND is the command to run.
Optional argument ARGS is a list of arguments.
Returns the buffer that holds the output from 'svn'."
  (let ((buf (get-buffer-create " *svn*"))
        (dir default-directory))
    (with-current-buffer buf
      (erase-buffer)
      (setq default-directory dir))
    (svn-call-svn nil buf nil (cons (symbol-name command) args))
    buf))

(defun svn-run-for-stdout (command args)
  "Run `svn', and return standard output as a string, discarding stderr.
Argument COMMAND is the svn subcommand to run.
Optional argument ARGS is a list of arguments."
  (let ((output-buffer (generate-new-buffer "*svn-stdout*")))
    (svn-call-svn nil (list output-buffer nil) nil
                  (cons (symbol-name command) args))
    (let ((stdout (with-current-buffer output-buffer (buffer-string))))
      (kill-buffer output-buffer)
      stdout)))

(defun svn-output-filter (proc str)
  "Output filter for svn output.
Argument PROC is the process object.
Argument STR is the output string."
  (save-excursion
    (set-buffer (process-buffer proc))
    (goto-char (process-mark proc))
    ;; put point back where it was to avoid scrolling for long output
    ;; (e.g., large diffs)
    (let ((p (point))
          (inhibit-read-only t))
      (insert-before-markers str)
      (goto-char p))))

(defvar svn-todo-queue '()
  "A queue of commands to run when the current command finishes.")
(make-variable-buffer-local 'svn-todo-queue)

(defun svn-current-url ()
  "Get the repository URL."
  (with-current-buffer (svn-run-hidden 'info ())
    (if (re-search-backward "^URL: \\(.*\\)$" nil t)
        (match-string 1)
      (error "Couldn't find the current URL"))))

(defun svn-repository-root ()
  "Get the repository root."
  (with-current-buffer (svn-run-hidden 'info ())
    (if (re-search-backward "^Repository Root: \\(.*\\)$" nil t)
        (match-string 1)
      (error "Couldn't find the repository root"))))

(defconst svn-noninteractive-blacklist
  '(add revert resolved)
  "Subversion commands that don't accept the --non-interactive option.
This is only important for svn 1.4, as 1.5 accepts this option for all
commands.") 

(defun svn-run (command args &optional description)
  "Run subversion command COMMAND with ARGS.

Optional third argument DESCRIPTION is a string used in the status
buffer to describe what is going on."
  ;; Clean up old output
  (let ((inhibit-read-only t))
    (delete-region svn-output-marker (point-max)))
  
  (let* ((command-s (symbol-name command))
         (filter-func (intern (concat "svn-" command-s "-filter")))
         (sentinel-func (intern (concat "svn-" command-s "-sentinel")))
         proc)
    ;; The command status-v is interpreted as status -v
    (when (eq command 'status-v)
      (setq command-s "status"
            args (cons "-v" args)))
    (unless (memq command svn-noninteractive-blacklist)
      (setq args (cons "--non-interactive" args)))
    (setq proc (svn-start-svn-process (current-buffer) (cons command-s args)))
    (if (fboundp filter-func)
        (set-process-filter proc filter-func)
      (set-process-filter proc 'svn-default-filter))
    (if (fboundp sentinel-func)
        (set-process-sentinel proc sentinel-func)
      (set-process-sentinel proc 'svn-default-sentinel))
    (setq svn-running (list description proc))
    (set-svn-process-status 'running)
    proc))

(defun svn-check-running ()
  (when svn-running
    (error "Can't run two svn processes from the same buffer")))

(defun svn-run-async (command args &optional file-filter)
  "Run subversion command COMMAND with ARGS, possibly at a later time.

Optional third argument FILE-FILTER is the file filter to be in effect
during the run."

  (if svn-running
      (setq svn-todo-queue
	    (nconc svn-todo-queue
		   (list (list command args file-filter))))
    (progn
      (set (make-local-variable 'svn-file-filter) file-filter)
      (svn-run command args))))

;; This could be used to debug filter functions
(defvar svn-output-queue nil)
(defvar svn-in-output-filter nil)
(defun svn-filter-queue (proc str)
  (setq svn-output-queue (nconc svn-output-queue (list str)))
  (unless svn-in-output-filter
    (let ((svn-in-output-filter t))
      (while svn-output-queue
        (svn-status-filter proc (car svn-output-queue))
        (setq svn-output-queue (cdr svn-output-queue))))))

(defun svn-default-filter (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert str))))

(defun svn-default-sentinel (proc reason)
  (with-current-buffer (process-buffer proc)
    (when (and svn-running
               (eq proc (cadr svn-running)))
      (setq svn-running nil)
      (if (/= (process-exit-status proc) 0)
          (set-svn-process-status 'failed)
        (set-svn-process-status 'finished))
      (move-to-column goal-column))
    (when svn-todo-queue
      (let ((cmd-info (car svn-todo-queue)))
        (setq svn-todo-queue (cdr svn-todo-queue))
	(let ((command (car cmd-info))
	      (args (cadr cmd-info))
	      (file-filter (car (cddr cmd-info))))
	  (set (make-local-variable 'svn-file-filter) file-filter)
	  (svn-run command args))))))

(defun svn-diff (arg)
  "Run `svn diff'.
Argument ARG are the command line arguments."
  (interactive "ssvn diff arguments: ")
  (svn-run-with-output "diff"
		       (append svn-diff-args (split-string arg))
		       'diff-mode))

(defun svn-add-unversioned-files-p (files)
  "Ask the user whether FILES should be added; return the answer."
  (let ((buf (get-buffer-create "*svn-unversioned-files*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (mapconcat (lambda (f) f) files "\n"))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (save-selected-window
        (pop-to-buffer buf)
        (shrink-window-if-larger-than-buffer)
        (let* ((n (length files))
               (add-them (y-or-n-p
                          (if (= n 1)
                              "Add this item first? "
                            (format "Add these %d items first? " n)))))
          (let ((win (get-buffer-window buf)))
            (if win
                (condition-case nil
                    (delete-window win)
                  (error nil))))
          (bury-buffer buf)
          add-them)))))

(defun svn-commit ()
  "Commit changes to one or more files."
  (interactive)
  (save-some-buffers)
  (let ((unversioned-files (svn-action-files
                            (lambda (pos) (eq (svn-file-status pos) ?\?)))))
    (if unversioned-files
        (if (svn-add-unversioned-files-p unversioned-files)
            (progn
              (message "Adding unversioned items. Please re-commit when ready.")
              (svn-run 'add unversioned-files "Adding files"))
          (message "Files not added; nothing committed."))
      (let ((status-buf (current-buffer))
            (commit-buf (get-buffer-create "*svn commit*"))
            (window-conf (and svn-restore-windows
                              (current-window-configuration)))
            (listfun (lambda () (with-current-buffer log-edit-parent-buffer
                                  (svn-action-files)))))
        (log-edit 'svn-confirm-commit t
                  (if (< emacs-major-version 23)
                      listfun
                    (list (cons 'log-edit-listfun listfun)))
                  commit-buf)
        (set (make-local-variable 'saved-window-configuration) window-conf)))))

(defun svn-confirm-commit ()
  "Commit changes with the current buffer as commit message."
  (interactive)
  (let ((files (with-current-buffer log-edit-parent-buffer
                 (svn-action-files)))
        (commit-buf (current-buffer))
        (status-buf log-edit-parent-buffer)
        (window-conf saved-window-configuration)
	;; XEmacs lacks make-temp-file but has make-temp-name + temp-directory
        (msg-file (if (fboundp 'make-temp-file)
		      (make-temp-file "svn-commit")
		    (make-temp-name (expand-file-name "svn-commit"
						      (temp-directory))))))
    ;; Ensure final newline
    (goto-char (point-max))
    (unless (bolp)
      (newline))
    (write-region (point-min) (point-max) msg-file)
    (when (boundp 'vc-comment-ring)
      ;; insert message into comment ring, unless identical to the previous
      (let ((comment (buffer-string)))
	(when (or (ring-empty-p vc-comment-ring)
		  (not (equal comment (ring-ref vc-comment-ring 0))))
	  (ring-insert vc-comment-ring comment))))
    (kill-buffer commit-buf)
    (with-current-buffer status-buf
      (make-local-variable 'svn-commit-msg-file)
      (make-local-variable 'svn-commit-files)
      (setq svn-commit-msg-file msg-file)
      (setq svn-commit-files files)
      (svn-run 'commit (append (list "-N" "-F" msg-file) files)))
    (if window-conf
        (set-window-configuration window-conf))))

(defun svn-commit-filter (proc str)
  "Output filter function for `svn commit'."
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t)
          (nomore))
      (goto-char (point-max))
      (insert str)
      (goto-char svn-output-marker)
      (while (not nomore)
        (cond ((looking-at
                "\\(Sending\\|Adding\\|Transmitting file\\|Deleting\\) .*\n")
               ;; Ignore these expected and uninteresting messages
               (delete-region (match-beginning 0)
                              (match-end 0)))
              ((looking-at "Committed revision \\([0-9]+\\).\n")
               (svn-update-label svn-revision-label (match-string 1))
               (forward-line 1))
              ((looking-at ".*\n")
               ;; Unexpected output is left in the buffer
               (forward-line 1))
              (t
               (setq nomore t)))))))

(defun svn-commit-sentinel (proc reason)
  "Sentinel function for `svn commit'."
  (with-current-buffer (process-buffer proc)
    (when (= (process-exit-status proc) 0)
      (while svn-commit-files
        (let* ((file (car svn-commit-files))
               (path (concat default-directory file))
               (pos (svn-file-pos file))
               (file-buffer (get-file-buffer path))
               (inhibit-read-only t))
          (when pos
            (svn-update-status-flag pos ?\  ?\ )
            (svn-update-status-msg pos "Committed"))
          (when (and file-buffer (fboundp 'vc-svn-workfile-version))
            (with-current-buffer file-buffer
              ;; Use buffer-file-name instead of path to get the
              ;; canonical file name used by vc
              ;; TODO: use the version number written by the commit command
              (vc-file-setprop buffer-file-name 'vc-workfile-version
                               (vc-svn-workfile-version buffer-file-name))
              (vc-mode-line buffer-file-name))))
        (setq svn-commit-files (cdr svn-commit-files))))
    (delete-file svn-commit-msg-file))
  (svn-default-sentinel proc reason))

;;; Svn log

(defun svn-file-log (pos)
  "List the change log of the selected file or directory."
  (interactive "d")
  (let ((file (or (svn-getprop pos 'file)
                  (svn-getprop pos 'dir))))
    (unless file
      (error "No file or directory on this line"))
    (svn-log (list file))))

(defun svn-log (arg)
  "Run `svn log'.
Argument ARG is the command-line arguments, as a string or a list."
  (interactive "ssvn log arguments: ")
  (when (stringp arg)
    (setq arg (split-string arg)))
  (svn-run-with-output "log" arg
                       'svn-log-mode))

(defvar svn-log-mode-map nil
  "Keymap for `svn-log-mode'.")
(unless svn-log-mode-map
  (setq svn-log-mode-map (make-sparse-keymap))
  (define-key svn-log-mode-map "\r" 'svn-log-show-diff)
  (define-key svn-log-mode-map "n" 'svn-log-next)
  (define-key svn-log-mode-map "p" 'svn-log-prev)
  (define-key svn-log-mode-map "e" 'svn-log-edit)
  (define-key svn-log-mode-map "+" 'svn-log-expand)
  (define-key svn-log-mode-map "-" 'svn-log-compact)
  (define-key svn-log-mode-map "=" 'svn-log-diff)
  )

(defun svn-update-log-entry (verbose-p)
  "Update the log entry under point, using verbose output if
VERBOSE-P."
  (save-excursion
    (end-of-line)
    (re-search-backward svn-log-entry-start-re nil t)
    (let ((start (point)))
      (unless (re-search-forward "^r\\([0-9]+\\) |" nil t)
        (error "Found no commit"))
      (let* ((commit-id (string-to-int (match-string 1)))
             (new (svn-run-hidden 'log
                                  (append (and verbose-p '("-v"))
                                          '("-r")
                                          (list (int-to-string commit-id)))))
             (text (with-current-buffer new
                     (goto-char (point-min))
                     (unless (re-search-forward svn-log-entry-start-re nil t)
                       (error "Failed finding log entry start"))
                     (unless (re-search-forward svn-log-entry-start-re nil t)
                       (error "Failed finding log entry end"))
                     (buffer-substring (point-min) (match-beginning 0))))
             (inhibit-read-only t))
        (re-search-forward svn-log-entry-start-re nil 'limit)
        (goto-char (match-beginning 0))
        (delete-region start (point))
        (insert text)))))

(defun svn-log-expand ()
  "Show verbose log entry information."
  (interactive)
  (svn-update-log-entry t))

(defun svn-log-compact ()
  "Show compact log entry information."
  (interactive)
  (svn-update-log-entry nil))

(defun svn-log-mode ()
  "Major mode for viewing Subversion logs.

\\{svn-log-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'svn-log-mode
        mode-name "Svn log")
  (use-local-map svn-log-mode-map)
  (setq paragraph-start "^commit"))

(defconst svn-log-entry-start-re "^-\\{72\\}$")

(defun svn-log-find-revision (commit-id)
  (let (found start)
    (save-excursion
      (goto-char (point-min))
      (while (and (re-search-forward svn-log-entry-start-re nil t)
                  (setq start (point))
                  (re-search-forward "^r\\([0-9]+\\) |" nil t)
                  (if (/= (string-to-int (match-string 1)) commit-id)
                      t
                    (setq found t)
                    nil))))
    (when found
      (goto-char start)
      (beginning-of-line)
      t)))

(defun svn-log-current-commit ()
  (save-excursion
    (end-of-line)
    (re-search-backward svn-log-entry-start-re nil t)
    (unless (re-search-forward "^r\\([0-9]+\\) |" nil t)
      (error "Found no commit"))
    (string-to-int (match-string 1))))

(defun svn-log-show-diff ()
  "Show the changes introduced by the changeset under point."
  (interactive)
  (let ((commit-id (svn-log-current-commit))
        (diff-buf (get-buffer-create "*svn diff*"))
        (dir default-directory)
        (inhibit-read-only t))
    (display-buffer diff-buf)
    (with-current-buffer diff-buf
      (diff-mode)
      (setq buffer-read-only t)
      (erase-buffer)
      (setq default-directory dir)
      (svn-call-process diff-buf
       (append (list "diff" "-r" (format "%d:%d" (1- commit-id) commit-id))
               svn-diff-args)))))

(defun svn-log-edit-files (commit-id)
  (let ((root (svn-repository-root))
        result)
    (with-current-buffer
        (svn-run-hidden 'log (list "-v" "-r"
                                    (int-to-string commit-id)
                                    root))
      (goto-char (point-min))
      (unless (re-search-forward "^Changed paths:" nil t)
        (error "Cannot find list of changes"))
      (while (re-search-forward
              "^   \\(\\S-+\\)\\s-+\\(.*?\\)\\( (from .*)$\\)?$"
              nil t)
        (let ((how (match-string 1))
              (file (match-string 2))
              (tail (match-string 3)))
          (when (string-match "\\([^/]*/\\)?\\([^/]*\\)$" file)
            (setq file (match-string 0 file)))
          (setq result (cons (concat how " " file) result)))))
    (nreverse result)))

(defun svn-log-diff ()
  "Run `svn diff' for the current log entry."
  (interactive)
  (let ((commit-id (svn-log-current-commit)))
    (svn-run-with-output "diff"
			 (append svn-diff-args
				 (list "-c" (number-to-string commit-id)))
                         'diff-mode)))

(defun svn-log-edit ()
  "Edit the log message for the revision under point."
  (interactive)
  (let* ((commit-id (svn-log-current-commit))
	 (log (svn-propget commit-id "svn:log"))
         (cwd default-directory)
         (parent-buffer (current-buffer))
	 (buffer (get-buffer-create (format "*svn log message of r%d*"
					    commit-id))))
    (log-edit 'svn-log-edit-done t
              `(lambda () (svn-log-edit-files ,commit-id))
              buffer)
    (insert log)
    (set (make-local-variable 'svn-commit-id) commit-id)
    (set (make-local-variable 'svn-directory) cwd)
    (set (make-local-variable 'svn-parent-buffer) parent-buffer)
    (setq default-directory cwd)
    (message (substitute-command-keys
	      "Press \\[log-edit-done] when you are done editing."))))

(defun svn-log-edit-done ()
  (interactive)
  (setq default-directory svn-directory) ; just in case the user cd'd
  (message "Changing log message...")
  (let ((commit-id svn-commit-id))
    (svn-propset commit-id "svn:log" (buffer-string))
    (when (buffer-name svn-parent-buffer)
      (save-excursion
        (set-buffer svn-parent-buffer)
        (when (svn-log-find-revision commit-id)
          (svn-update-log-entry nil)))))
  (kill-buffer nil)
  (message "Changing log message... done"))

(defun svn-log-next ()
  "Move to the next changeset in the log."
  (interactive)
  (end-of-line)
  (unless (re-search-forward svn-log-entry-start-re nil t)
    (error "Found no commit"))
  (beginning-of-line)
  (svn-log-show-diff))

(defun svn-log-prev ()
  "Move to the previous changeset in the log."
  (interactive)
  (beginning-of-line)
  (unless (re-search-backward svn-log-entry-start-re nil t)
    (error "Found no commit"))
  (svn-log-show-diff))

(defun svn-new-label (&optional pos)
  (unless pos (setq pos (point)))
  (let ((start (make-marker))
        (stop (make-marker)))
    (set-marker start pos)
    (set-marker stop pos)
    (list start stop)))

(defun svn-update-label (label str)
  (let ((start (car label))
        (stop (cadr label))
        (inhibit-read-only t))
    (delete-region start stop)
    (set-marker-insertion-type stop t)
    (save-excursion
      (goto-char start)
      (insert str))))

;;; Svn propedit

(defun svn-prop-args (file-or-rev)
  "Returns a list of arguments to the 'svn prop...' commands, to
make them act on FILE-OR-REV (a file name or a revision number)."
  (if (integerp file-or-rev)
      (list "--revprop" "-r" (int-to-string file-or-rev))
    (list file-or-rev)))

(defun svn-prop-description (file-or-rev)
  "Returns a human-readable description of FILE-OR-REV (a file
name or revision number)."
  (if (integerp file-or-rev)
      (format "revision %d" file-or-rev)
    file-or-rev))

(defun svn-propget (file-or-rev propname)
  "Return the Subversion property PROPNAME of FILE-OR-REV (file
name or revision number)."
  (with-current-buffer
      (svn-run-hidden 'propget
		      (cons propname
			    (svn-prop-args file-or-rev)))
    (substring (buffer-string) 0 -1)))	; trim final newline added by svn

(defun svn-get-props (file)
  "Return an alist containing the properties of FILE"
  ;; First retrieve the property names, and then the value of each.
  ;; We can't use proplist -v because is output is ambiguous when values
  ;; consist of multiple lines.
  (if (string-equal (svn-run-for-stdout 'info (list file)) "")
    (error "%s is not under version control" file))
  (let (propnames)
    (with-current-buffer (svn-run-hidden 'proplist (list file))
      (goto-char (point-min))
      (when (looking-at "Properties on ")
	(forward-line 1)
	(while (looking-at "  \\(.+\\)$")
	  (setq propnames (cons (match-string 1) propnames))
	  (forward-line 1))))
    (mapcar (lambda (propname)
	      (cons propname (svn-propget file propname)))
	    propnames)))

(defun svn-propedit (file)
  "Edit properties of FILE."
  (interactive (list (expand-file-name
		      (or (svn-getprop (point) 'file)
			  (read-file-name "Edit properties of file: "
					  default-directory
					  nil t
					  (svn-getprop (point) 'dir))))))
  (let ((local-file (svn-local-file-name file)))
    (when (string-equal local-file "")
	(setq local-file ".")
	(setq file (file-name-as-directory file)))
    (svn-check-running)
    (let ((buf-name (format "*propedit %s*" local-file)))
      (if (get-buffer buf-name)
	  (kill-buffer buf-name))
      (let ((prop-alist (svn-get-props local-file))
	    (propedit-buf (get-buffer-create buf-name)))
	(switch-to-buffer-other-window propedit-buf)
	(svn-propedit-mode)
	(insert
      "# Properties of " local-file "\n"
      "#\n"
      "# Lines are on the form PROPNAME: VALUE for single-line values,\n"
      "# or just PROPNAME: followed by one or more lines starting with > for\n"
      "# multi-line values. Lines starting with # are ignored.\n"
      "#\n"
      "# Change, add, delete or rename properties just by editing this\n"
      "# buffer; then press "
         (substitute-command-keys "\\[svn-propedit-done]")
	 " to save changes.\n\n")
	(mapc (lambda (prop)
		(let* ((value (cdr prop))
		       (lines nil)
		       (len (length value))
		       (ofs 0))
		  ;; Split value in lines - we can't use split-string because
		  ;; its behaviour is not consistent across Emacs versions.
		  (while (<= ofs len)
		    (let* ((nl (or (string-match "\n" value ofs) len)))
		      (setq lines (cons (substring value ofs nl) lines))
		      (setq ofs (+ nl 1))))
		  (setq lines (nreverse lines))
		  ;; The lines list now contains one string per line, and
		  ;; an empty list at the end if the string finished in a \n.

		  (insert (car prop) ":")
		  (if (> (length lines) 1)
		      (progn
			(insert "\n")
			(mapc (lambda (line) (insert ">" line "\n"))
			      lines))
		    (insert " " (or (car lines) "") "\n"))))
	      (sort prop-alist #'(lambda (a b) (string< (car a) (car b)))))
	(make-local-variable 'svn-propedit-file)
	(setq svn-propedit-file file)
	(setq default-directory (file-name-directory file))
	(message
	 (substitute-command-keys
	  "Press \\[svn-propedit-done] when you are done editing."))))))

(defvar svn-propedit-mode-map nil
  "Keymap for `svn-propedit-mode'.")
(unless svn-propedit-mode-map
  (setq svn-propedit-mode-map (make-sparse-keymap))
  (define-key svn-propedit-mode-map "\C-c\C-c" 'svn-propedit-done))

(defun svn-propedit-mode ()
  "Major mode for editing Subversion properties."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'svn-propedit-mode
	mode-name "Svn propedit")
  (use-local-map svn-propedit-mode-map)
  (setq font-lock-defaults
	'((("^#.*$"			       ;comment
	    . 'font-lock-comment-face)
	   ("^\\([^ \t\n#>][^ \t\n]*\\):"      ;property name
	    . (1 'bold))
	   ("^[^ \t\n#>][^ \t\n]*: *\\(.*\\)$" ;property value
	    . (1 'font-lock-function-name-face))
	   ("^>"			       ;multi-line marker
	    . 'bold)
	   ("^>\\(.*\\)$"		       ;property value (continued)
	    . (1 'font-lock-function-name-face))
	   )
	  nil				;keywords-only
	  nil				;case-fold
	  ;; syntax-alist: don't fontify quotes specially in any way
	  ((?\" . "."))
	  nil				;syntax-begin
	  ))
  (font-lock-mode))

(defun svn-props-from-buffer ()
  "Parse the current propedit buffer and return an alist of the properties."
  (save-excursion
    (let (prop-alist)
      (goto-char (point-min))
      (while (not (eobp))
	(cond ((looking-at "^\\([^ \t\n#>][^ \t\n]*\\): *\\(.*\\)$")
	       (let ((prop-name (match-string 1))
		     (value (match-string 2)))
		 (set-text-properties 0 (length prop-name) nil prop-name)
		 (set-text-properties 0 (length value) nil value)
		 (when (assoc prop-name prop-alist)
		   (error "Duplicated property %s" prop-name))
		 (setq prop-alist (cons (cons prop-name value) prop-alist))))
	      ((looking-at "^>\\(.*\\)$")
	       (let ((extra-line (match-string 1)))
		 (set-text-properties 0 (length extra-line) nil extra-line)
		 (when (null prop-alist)
		   (error "Continued line not preceded by property name"))
		 (let ((old-value (cdar prop-alist)))
		   (setcdr (car prop-alist)
			   (concat old-value "\n" extra-line))))))
	(forward-line 1))

      ;; Remove the extra leading newline from multi-line values
      (mapcar (lambda (prop)
		(let ((name (car prop))
		      (value (cdr prop)))
		  (if (and (not (equal value ""))
			   (equal (substring value 0 1) "\n"))
		      (cons name (substring value 1))
		    prop)))
	      prop-alist))))

(defun svn-propdel (file prop-name)
  "Delete FILE's property PROP-NAME."
  (svn-run-hidden 'propdel (list prop-name file)))

(defun svn-propset (file-or-rev prop-name prop-value)
  "Set the property PROP-NAME to PROP-VALUE for FILE-OR-REV (a
file name or revision number)."
  (let ((buf (svn-run-hidden 'propset (append (list prop-name prop-value)
                                              (svn-prop-args file-or-rev)))))
    (unless
        (with-current-buffer buf
          (goto-char (point-min))
          (looking-at "^property '.*' set on "))
      (switch-to-buffer buf)
      (error "Failed setting property %s of %s"
             prop-name
             (svn-prop-description file-or-rev)))))

(defun svn-propedit-done ()
  "Apply property changes to the file."
  (interactive)
  (let ((wc-props (svn-get-props svn-propedit-file))
	(new-props (svn-props-from-buffer))
	(changes 0))

    ;; first remove properties that the user deleted from the buffer
    (mapc (lambda (wc-prop)
	    (let ((prop-name (car wc-prop)))
	      (when (not (assoc prop-name new-props))
		(message "Deleting property %s" prop-name)
		(svn-propdel svn-propedit-file prop-name)
		(setq changes (1+ changes)))))
	  wc-props)

    ;; then set the properties that have changed or are new
    (mapc (lambda (new-prop)
	    (let* ((prop-name (car new-prop))
		   (wc-prop (assoc prop-name wc-props)))
	      (unless (equal new-prop wc-prop)
		(message "Setting property %s" prop-name)
		(svn-propset svn-propedit-file prop-name (cdr new-prop))
		(setq changes (1+ changes)))))
	  new-props)
    (cond
     ((> changes 1) (message "Changed %d properties." changes))
     ((= changes 0) (message "No properties changed."))))
  (svn-foreach-svn-buffer
   svn-propedit-file
   (lambda (local-file-name file-pos)
     (svn-refresh-item local-file-name nil)))
  (kill-buffer (current-buffer)))

;;; Svn buffer

(defvar svn-files-start nil)
(defvar svn-files-stop nil)
(defvar svn-url-label nil)
(defvar svn-revision-label nil)
(defvar svn-running-label nil)
(defvar svn-output-marker nil)

(defvar svn-running nil)

(defun create-svn-buffer (dir)
  "Create a buffer for showing svn status.
Argument DIR is the directory to run svn in."
  (let ((status-buf (create-file-buffer (concat dir "*svn*")))
        (inhibit-read-only t))
    (with-current-buffer status-buf
      (setq default-directory dir)
      (svn-status-mode)

      (make-local-variable 'svn-url-label)
      (make-local-variable 'svn-revision-label)
      (make-local-variable 'svn-running-label)
      (make-local-variable 'svn-output-marker)

      (insert "Svn status for " dir) (newline)
      (insert "URL: ") (setq svn-url-label (svn-new-label))
      (insert " revision " ) (setq svn-revision-label (svn-new-label))
      (newline)
      (newline)
      (insert "---- ") (setq svn-running-label (svn-new-label))
      (newline)
      (setq svn-files-start (point-marker))
      (set-marker-insertion-type svn-files-start nil)
      (setq svn-last-inserted-marker (point-marker))
      (set-marker-insertion-type svn-last-inserted-marker nil)
      (insert "----")
      (newline)
      (setq svn-output-marker (point-marker))
      (set-marker-insertion-type svn-output-marker nil)
      ;; Do this after inserting stuff
      (setq svn-files-stop (copy-marker svn-files-start t))
      (setq buffer-read-only t))
    status-buf))

(defun switch-to-svn-buffer (dir)
  "Switch to a (possibly new) buffer displaying status for DIR"
  (setq dir (file-name-as-directory dir))
  (let ((buffers (buffer-list)))
    (while (and buffers
                (not (with-current-buffer (car buffers)
                       (and (eq major-mode 'svn-status-mode)
                            (string= default-directory dir)))))
      (setq buffers (cdr buffers)))
    (switch-to-buffer (if buffers
                          (car buffers)
                        (create-svn-buffer dir)))))

(defun svn-in-dir-p (dir file)
  "Return non-NIL if FILE is inside DIR"
  (let ((l (length dir)))
    (and (> (length file) l)
         (string= dir (substring file 0 l)))))

;;; Svn status

(defun svn-status (dir)
  "Run `svn status'.
Argument DIR is the directory to run svn status in."
  (interactive "DDirectory: \n")
  (switch-to-svn-buffer dir)
  (let ((proc (svn-run 'info ())))
    (while (eq (process-status proc) 'run)
      (accept-process-output proc 2 10000)))
  ;; The sentinel isn't run by a-p-o, so we hack around it
  (setq svn-running nil)
  (svn-refresh)
  (message
   (substitute-command-keys
    "Welcome to dsvn. Press \\[svn-status-help] for keyboard help summary.")))

(defun svn-refresh (&optional clear)
  "Run `svn status'.
If optional prefix argument CLEAR is non-NIL, clear the buffer first."
  (interactive "P")
  (svn-check-running)
  (let ((inhibit-read-only t))
    (if clear
        (delete-region svn-files-start svn-files-stop)
      (put-text-property svn-files-start svn-files-stop 'svn-updated nil))
    (setq svn-last-inserted-filename nil)
    (svn-run 'status '())))

(defun svn-run-status-v (files recursive)
  "Run svn status -v on FILES. If not RECURSIVE, only applies to files and
directories explicitly listed in FILES."

  ;; The command 'svn status -N DIR' will process the immediate contents of
  ;; DIR as well as DIR itself, but that is not what we want if running
  ;; non-recursively. To compensate, filter the status output through a list
  ;; of files and directories we are interested in.

  (let ((flag (if recursive nil '("-N")))
	(file-filter
	 (if recursive
	     nil
	   (mapcar (lambda (file)
		     ;; trim trailing slash for directory comparison to work
		     (if (equal (substring file -1) "/")
			 (substring file 0 -1)
		       file))
		   files))))
    (svn-run-async 'status-v (append flag files) file-filter)))

(defun svn-refresh-file ()
  "Run `svn status' on the selected files."
  (interactive)
  (svn-check-running)
  (let ((actions (svn-actions))
        (inhibit-read-only t))
    (setq svn-last-inserted-filename nil)
    (put-text-property svn-files-start svn-files-stop 'svn-updated t)
    (mapc (lambda (pos)
            (svn-setprop pos 'updated nil))
          (mapcar 'cadr actions))
    (svn-run-status-v (mapcar 'car actions) t))
  (svn-next-file 1))

(defun svn-local-file-name (file)
  "Return file name relative the current directory, or raise an error if
outside."
  (if (file-directory-p file)
      (setq file (file-name-as-directory file)))
  (let ((exp-default-dir (expand-file-name default-directory)))
    (if (file-name-absolute-p file)
	(let ((ddl (length exp-default-dir)))
	  (if (or (< (length file) ddl)
		  (not (string= (substring file 0 ddl)
				exp-default-dir)))
	      (error "Outside working copy")
	    (substring file ddl)))
      file)))

(defun svn-refresh-item (file recursive)
  "Refresh status for FILE. If RECURSIVE, do it recursively (for directories)."
  (svn-check-running)
  (let ((inhibit-read-only t))
    (setq svn-last-inserted-filename nil)
    (let ((local-file (svn-local-file-name file)))
      (svn-run-status-v (list local-file) recursive))))

(defun svn-refresh-one (file)
  "Run `svn status' on FILE."
  (interactive (list (expand-file-name
		      (read-file-name "Svn status on file: "
				      default-directory
				      nil t
				      (or (svn-getprop (point) 'file)
					  (svn-getprop (point) 'dir))))))
  (svn-refresh-item file t))

(defun svn-cleanup-status ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char svn-files-start)
      (while (< (point) svn-files-stop)
        (if (or (svn-getprop (point) 'dir)
                (svn-getprop (point) 'updated))
            (forward-line)
          (svn-update-status-flag (point) ?\  ?\ )
          (svn-update-status-msg (point) "")
          (forward-line))))))

;; Translate backslashes to forward slashes, because that is what
;; Emacs uses internally even on Windows and it permits us to compare
;; file name strings.
(defun svn-normalise-path (path)
  (replace-regexp-in-string "\\\\" "/" path t t))

(defun svn-status-filter (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert str)
      (goto-char svn-output-marker)
      (while
	 (cond
	  ((looking-at
	    "\\([ ACDGIMRX?!~][ CM][ L][ +][ S][ KOTB]\\)[ C]? \\([^ ].*\\)\n")
	   (let ((status (match-string 1))
		 (filename (svn-normalise-path (match-string 2))))
	     (delete-region (match-beginning 0) (match-end 0))
	     (svn-insert-file filename status))
	   t)
	  ((looking-at "\n\\|Performing status on external item at .*\n")
	   (delete-region (match-beginning 0) (match-end 0))
	   t)
	  ((looking-at "      > +\\([^ ].*\\)\n")
	   (let ((tree-conflict (match-string 1)))
	     (delete-region (match-beginning 0) (match-end 0))
	     (svn-update-status-msg svn-last-inserted-marker "TConflict")
	     (svn-update-conflict-msg svn-last-inserted-marker tree-conflict))
	   t))))))

(defun svn-status-sentinel (proc reason)
  (with-current-buffer (process-buffer proc)
    (svn-cleanup-status)
    (svn-insert-dirs))
  (svn-default-sentinel proc reason))

(defun svn-status-v-filter (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert str)
      (goto-char svn-output-marker)
      (while
	  (cond
	   ((looking-at
	     "\\(?:\\(\\?.....\\)\\|\\([ ACDGIMRX!~][ CM][ L][ +][ S][ KOTB]\\)[ C]? [* ] +[^ ]+ +[^ ]+ +[^ ]+\\) +\\([^ ].*\\)\n")
	    (let ((status (or (match-string 1) (match-string 2)))
		  (filename (svn-normalise-path (match-string 3))))
	      (delete-region (match-beginning 0) (match-end 0))
	      (when (or (not svn-file-filter)
			(member filename svn-file-filter))
		(svn-insert-file filename status)))
	    t)
	   ((looking-at "      > +\\([^ ].*\\)\n")
	    (let ((tree-conflict (match-string 1)))
	      (delete-region (match-beginning 0) (match-end 0))
	      (svn-update-status-msg svn-last-inserted-marker "TConflict")
	      (svn-update-conflict-msg svn-last-inserted-marker tree-conflict))
	    t))))))

(defun svn-status-v-sentinel (proc reason)
  (with-current-buffer (process-buffer proc)
    (svn-cleanup-status))
  (svn-default-sentinel proc reason))

;; info

(defun svn-info-filter (proc str)
  "Output filter function for `svn info'."
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t)
          (nomore))
      (goto-char (point-max))
      (insert str)
      (goto-char svn-output-marker)
      (while (not nomore)
        (cond ((looking-at "URL: \\(.*\\)\n")
               (svn-update-label svn-url-label (match-string 1))
               (forward-line 1))
              ((looking-at "Revision: \\([0-9]+\\)\n")
               (svn-update-label svn-revision-label (match-string 1))
               (forward-line 1))
              ((looking-at ".*\n")
               ;; Unexpected output is left in the buffer
               (forward-line 1))
              (t
               (setq nomore t)))))))

;; update

(defun svn-update (dir)
  "Run `svn update'.
Argument DIR is the directory to run svn status in."
  (interactive "DDirectory: \n")
  (switch-to-svn-buffer dir)
  (svn-update-current))

(defun svn-update-current (&optional revision)
  "Run `svn update' in the current buffer.
Update to REVISION, which defaults to HEAD.
With prefix arg, prompt for REVISION."
  (interactive (list
                (if current-prefix-arg
                    (read-string "update to revision (HEAD): "
                                 nil nil "HEAD")
                  nil)))
  (svn-check-running)
  (make-local-variable 'svn-updated-files)
  (setq svn-updated-files nil)
  (let ((args (if revision
                  (list "-r" revision)
                '())))
    (svn-run 'update args "Updating")))

(defconst svn-update-flag-name
  '((?A . "Added")
    (?D . "Deleted")
    (?U . "Updated")
    (?G . "Merged")
    (?C . "Conflict")
    (?E . "Existed")))

(defvar svn-merging nil)

(defun svn-remap-update-to-status (status-char)
  "Map a status character from the svn update command to the resulting status."
  (if svn-merging
      (cond ((memq status-char '(?U ?G))
             ?M)
            (t
             status-char))
    (cond ((memq status-char '(?A ?D ?U))
           ?\ )
          ((eq status-char ?G)
           ?M)
          (t
           status-char))))

(defun svn-update-filter (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t)
          nomore)
      (goto-char (point-max))
      (insert str)
      (goto-char svn-output-marker)
      (while (not nomore)
        (cond ((looking-at
                "\\([ ADUCGE][ ADUCGE][ B]\\)\\([ C]?\\) \\([^ ].*\\)\n")
               (let* ((status (match-string 1))
                      (file-status (elt status 0))
                      (prop-status (elt status 1))
		      (tree-status (match-string 2))
                      (filename (svn-normalise-path (match-string 3))))
                 (delete-region (match-beginning 0)
                                (match-end 0))
                 (svn-insert-file
                  filename
                  ;; Remap A and U to unmodified in file and prop status
                  (format "%c%c...."
                          (svn-remap-update-to-status file-status)
                          (svn-remap-update-to-status prop-status))
                  ;; Optimize for some common cases
                  (cond ((string= tree-status "C")
			 "TConflict")
			((= prop-status ?\ )
                         (cdr (assq file-status svn-update-flag-name)))
                        ((= file-status ?\ )
                         (let ((s (format "P %s"
                                          (cdr (assq prop-status
                                                     svn-update-flag-name)))))
                           (if (> (length s) 9)
                               (substring s 0 9)
                             s)))
                        (t
                         status)))
                 (setq svn-updated-files (cons filename svn-updated-files))))
              ((looking-at "At revision \\([0-9]+\\)\\.\n")
               (svn-update-label svn-revision-label (match-string 1))
               (forward-line 1))
              ((looking-at "Updating '.*':\n")
               (delete-region (match-beginning 0) (match-end 0)))
              ((and (not svn-merging)
                    (looking-at "Updated to revision \\([0-9]+\\)\\.\n"))
               (svn-update-label svn-revision-label (match-string 1))
               (forward-line 1))
              ((looking-at ".*\n")
               ;; Unexpected output is left in the buffer
               (forward-line 1))
              (t
               (setq nomore t)))))))

(defun svn-update-sentinel (proc reason)
  (with-current-buffer (process-buffer proc)
    (svn-insert-dirs)
    (mapc #'svn-revert-if-needed svn-updated-files))
  (svn-default-sentinel proc reason))

(defun svn-revert-if-needed (filename)
  "Revert buffer visiting FILENAME if any, because the file is believed to have
been modified."
  (let ((buf (find-buffer-visiting filename)))
    (when (and buf (not (buffer-modified-p buf)))
      (with-current-buffer buf
	(let ((was-ro buffer-read-only))
	  (condition-case nil
	      (revert-buffer t t)
	    (error nil))
	  (when was-ro (toggle-read-only 1)))))))

(defun svn-complete-url (url pred all)
  (string-match "\\`\\(.*/\\)\\([^/]*\\)\\'" url)
  (let* ((base-url (match-string 1 url))
         (match-file (match-string 2 url))
         (match-len (length match-file))
         (files))
    (with-current-buffer (svn-run-hidden 'ls (list base-url))
      (goto-char (point-min))
      (while (looking-at ".+$")
        (let ((file (match-string 0)))
          (if (and (>= (length file) match-len)
                   (string= match-file (substring file 0 match-len)))
              (setq files (cons file files)))
          (forward-line 1))))
    (setq files (nreverse files))
    (cond (all
           (mapcar (lambda (f) (concat base-url f))
                   files))
          ((and (= (length files) 1)
                (string= (car files) match-file))
           t)
          (t
           (try-completion url
                           (mapcar (lambda (s) (cons (concat base-url s) nil))
                                   files)
                           pred)))))

(defvar svn-switch-history nil)

(defun svn-switch (url)
  "Run `svn switch'."
  (interactive (list (completing-read "Switch to (URL): "
                                      'svn-complete-url
                                      nil nil
                                      (svn-current-url)
                                      'svn-switch-history)))
  (svn-check-running)
  (make-local-variable 'svn-updated-files)
  (setq svn-updated-files nil)
  (svn-update-label svn-url-label url)
  (svn-run 'switch (list url)))

(defun svn-switch-filter (proc str)
  "Filter function for 'svn switch' output."
  ;; The output is identical(?) to svn update
  (svn-update-filter proc str))

(defun svn-switch-sentinel (proc reason)
  ;; switch is basically a glorified update
  (svn-update-sentinel proc reason))

;; merge

(defun svn-merge (from-url from-rev to-url to-rev)
  "Run `svn merge'."
  (interactive (list (completing-read "Merge from (URL): "
                                      'svn-complete-url
                                      nil nil
                                      (svn-current-url)
                                      'svn-switch-history)
                     (read-string "Merge from revision (HEAD): "
                                  nil nil "HEAD")
                     (completing-read "Merge to (URL): "
                                      'svn-complete-url
                                      nil nil
                                      (car svn-switch-history)
                                      'svn-switch-history)
                     (read-string "Merge to revision (HEAD): "
                                  nil nil "HEAD")))
  (svn-check-running)
  (make-local-variable 'svn-updated-files)
  (setq svn-updated-files nil)
  (svn-run 'merge (list (format "%s@%s" from-url from-rev)
                        (format "%s@%s" to-url to-rev))))

(defun svn-merge-filter (proc str)
  "Filter function for 'svn merge' output."
  ;; The output is similar to svn update
  (let ((svn-merging t))
    (svn-update-filter proc str)))

(defun svn-merge-sentinel (proc reason)
  ;; merge is basically a glorified update
  (svn-update-sentinel proc reason))


(defvar svn-last-inserted-filename nil)
(defvar svn-last-inserted-marker nil)

(defsubst svn-file-name< (fn1 fn2)
  "Compare two filenames, FN1 and FN2 and decide the sort order"
  (let ((dir1 (or (file-name-directory fn1) ""))
        (dir2 (or (file-name-directory fn2) "")))
    (cond ((and (< (length dir1) (length dir2))
                (string= dir1 (substring dir2 0 (length dir1))))
           t)
          ((and (> (length dir1) (length dir2))
                (string= dir2 (substring dir1 0 (length dir2))))
           nil)
          (t
           (string< fn1 fn2)))))
          
(defun svn-insert-file (filename status &optional info)
  (save-excursion
    (save-restriction
      (narrow-to-region (1- svn-files-start) svn-files-stop)
      (if svn-last-inserted-filename
          (goto-char svn-last-inserted-marker)
        ;; Move to the middle of the list and start there
        (let ((line-count (count-lines svn-files-start svn-files-stop)))
          (goto-char svn-files-start)
          (forward-line (/ line-count 2))
          (setq svn-last-inserted-filename
                (or (svn-getprop (point) 'file)
                    (svn-getprop (point) 'dir)))))
      ;; Scan for the place to insert the new file, or replace an
      ;; existing line
      (cond ((null svn-last-inserted-filename)
             nil)
            ((svn-file-name< filename svn-last-inserted-filename)
             ;;(if (not (bobp))
             ;;    (forward-line -1))
             (while (and (not (bobp))
                         (not (svn-file-name< (or (svn-getprop (point) 'file)
                                                  (svn-getprop (point) 'dir))
                                              filename)))
               (forward-line -1))
             (forward-line 1))
            (t
             (while (and (not (eobp))
                         (svn-file-name< (or (svn-getprop (point) 'file)
                                             (svn-getprop (point) 'dir))
                                         filename))
               (forward-line 1))))
      (let ((marked nil))
        (when (string= filename (svn-getprop (point) 'file))
          (setq marked (svn-getprop (point) 'mark))
          (delete-region (point) (progn (forward-line 1) (point))))
        (set-marker svn-last-inserted-marker (point))
        (insert " "
                (if info (format "%-9s " info) "          ")
                status
                (if marked " * " "   ")
                filename)
        (newline)
        (add-text-properties svn-last-inserted-marker (point)
                             (append (list 'svn-file filename
                                           'svn-updated t
                                           'svn-mark marked)
                                     (if marked
                                         (list 'face 'svn-mark-face)
                                       ()))))))
  (setq svn-last-inserted-filename filename))

(defun svn-remove-line (pos)
  (save-excursion
    (goto-char pos)
    (forward-line 1)
    (delete-region pos (point))))

(defun svn-insert-dirs ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region svn-files-start svn-files-stop)
      (goto-char (point-min))
      (let ((inhibit-read-only t)
            (current-dir nil))
        (while (not (eobp))
          (let ((dir (svn-getprop (point) 'dir)))
            (if dir
                (setq current-dir dir)
              (let* ((start (point))
                     (file (svn-getprop (point) 'file))
                     (dir (or (file-name-directory file)
                              "")))
                (when (not (string= dir current-dir))
                  (setq current-dir dir)
                  (if (string= dir "")
                      (insert "         Top-level directory:")
                    (insert "         Directory " dir ":"))
                  (newline)
		  ;; Next line only needed on XEmacs
		  (remove-text-properties start (point) '(svn-file nil))
                  (add-text-properties start (point)
                                       (list 'face 'bold
                                             'svn-dir dir))))))
          (forward-line 1))))))

(defun svn-file-pos (filename)
  "Move to the line containing file information for FILENAME."
  (let ((pos svn-files-start))
    (while (and pos
                (not (string= filename (get-text-property pos 'svn-file))))
      (setq pos (next-single-property-change pos 'svn-file)))
    pos))
  
(defun svn-update-file-status (filename status-char)
  (let ((inhibit-read-only t))
    (save-excursion
      (svn-goto-file filename)
      (beginning-of-line)
      (delete-char 1)
      (insert status-char))))

(defun set-svn-process-status (status)
  (let ((description (car svn-running)))
    (svn-update-label svn-running-label
                      (cond ((eq status 'running)
                             (format "%s..."
                                     (or description "Running")))
                            ((eq status 'finished)
                             "")        ; "Finished")
                            ((eq status 'failed)
                             "Failed")
                            (t
                             ""))))
  (cond ((eq status 'running)
         (setq mode-line-process ": running"))
        (t
         (setq mode-line-process nil))))

(defvar svn-status-mode-map nil
  "Keymap for `svn-status-mode'.")

(defun svn-status-set-default-mode-map ()
  (setq svn-status-mode-map (make-sparse-keymap))
  (define-key svn-status-mode-map "a" 'svn-add-file)
  (define-key svn-status-mode-map "c" 'svn-commit)
  (define-key svn-status-mode-map "f" 'svn-find-file)
  (define-key svn-status-mode-map [mouse-2] 'svn-mouse-find-file)
  (define-key svn-status-mode-map "\r" 'svn-find-file)
  (define-key svn-status-mode-map "g" 'svn-refresh)
  (define-key svn-status-mode-map "\M-u" 'svn-update-current)
  (define-key svn-status-mode-map " " 'svn-toggle-mark)
  (define-key svn-status-mode-map "m" 'svn-mark-forward)
  (define-key svn-status-mode-map "\177" 'svn-unmark-backward)
  (define-key svn-status-mode-map "u" 'svn-unmark-forward)
  (define-key svn-status-mode-map "\M-\177" 'svn-unmark-all)
  (define-key svn-status-mode-map "o" 'svn-find-file-other-window)
  (define-key svn-status-mode-map "r" 'svn-remove-file)
  (define-key svn-status-mode-map "=" 'svn-diff-file)
  (define-key svn-status-mode-map "p" 'svn-previous-file)
  (define-key svn-status-mode-map "n" 'svn-next-file)
  (define-key svn-status-mode-map "l" 'svn-file-log)
  (define-key svn-status-mode-map "s" 'svn-refresh-file)
  (define-key svn-status-mode-map "S" 'svn-refresh-one)
  (define-key svn-status-mode-map "x" 'svn-expunge)
  (define-key svn-status-mode-map "U" 'svn-revert)
  (define-key svn-status-mode-map "R" 'svn-resolve)
  (define-key svn-status-mode-map "M" 'svn-move)
  (define-key svn-status-mode-map "D" 'svn-insert-dirs)
  (define-key svn-status-mode-map "\M-s" 'svn-switch)
  (define-key svn-status-mode-map "\M-m" 'svn-merge)
  (define-key svn-status-mode-map "q" 'bury-buffer)
  (define-key svn-status-mode-map "?" 'svn-status-help)
  (define-key svn-status-mode-map "P" 'svn-propedit)
)

(unless svn-status-mode-map (svn-status-set-default-mode-map))

(defun svn-status-mode ()
  "Major mode for Subversion status buffers.

\\{svn-status-mode-map}"
  (interactive)
  (kill-all-local-variables)
  
  (make-local-variable 'svn-files-start)
  (make-local-variable 'svn-files-stop)
  (make-local-variable 'svn-last-inserted-marker)
  (make-local-variable 'svn-last-inserted-filename)
  (make-local-variable 'svn-running)

  (set (make-local-variable 'list-buffers-directory)
       (expand-file-name "*svn*"))

  (setq major-mode 'svn-status-mode
        mode-name "Svn status")
  (use-local-map svn-status-mode-map)
  (setq goal-column svn-status-mark-col))


(defun svn-goto-file (filename)
  (let ((pos (next-single-property-change (point-min) 'svn-file)))
    (while (and pos
                (not (string= (svn-getprop pos 'file) filename)))
      (setq pos (next-single-property-change pos 'svn-file)))
    (if pos
        (goto-char pos))))

(defsubst svn-getprop (pos prop)
  (get-text-property pos (intern (concat "svn-" (symbol-name prop)))))

(defsubst svn-setprop (pos prop value)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (let ((start (point)))
      (forward-line)
      (put-text-property start (point)
                         (intern (concat "svn-" (symbol-name prop)))
                         value))))

(defsubst svn-file-status (pos)
  "Get the file status for the file at POS."
  (char-after (+ pos svn-status-flags-col)))

(defsubst svn-prop-status (pos)
  "Get the property status for the file at POS."
  (char-after (+ pos svn-status-flags-col 1)))

(defface svn-mark-face
  '((((type tty) (class color))
     (:background "cyan" :foreground "black"))
    (((class color) (background light))
     (:background "yellow2"))
    (((class color) (background dark))
     (:background "darkblue"))
    (t (:inverse-video t)))
  "Face used to highlight marked files"
  :group 'dsvn)

(defun svn-highlight-line (mark)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (forward-line)
      (let ((end (point))
	    (prop (list 'face 'svn-mark-face)))
	(if mark
	    (add-text-properties start end prop)
	  (remove-text-properties start end prop))))))

(defun svn-set-mark (pos mark)
  "Update the mark on the status line at POS.
Set it if MARK is non-NIL, and clear it if MARK is NIL."
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (+ pos svn-status-mark-col))
      (delete-char 1)
      (insert-and-inherit (if mark "*" " "))
      (svn-highlight-line mark)
      (forward-line 1)
      (put-text-property pos (point) 'svn-mark mark))))

(defun svn-actions (&optional pred)
  "Return a list of lists (FILE POS) to act on.
Optional argument PRED is a predicate function that is called with POS as
argument."
  (let ((positions ()))
    (let ((pos (next-single-property-change (point-min) 'svn-file)))
      (while pos
        (when (get-text-property pos 'svn-mark)
          (setq positions (cons pos positions)))
        (setq pos (next-single-property-change pos 'svn-file))))
    (when (null positions)
      (unless (svn-getprop (point) 'file)
        (error "No file on this line"))
      (setq positions (list (line-beginning-position))))

    (let ((files ()))
      (mapc (lambda (pos)
              (when (or (not pred) (funcall pred pos))
                (setq files (cons (list (get-text-property pos 'svn-file)
                                        pos)
                                  files))))
            (reverse positions))
      (reverse files))))

(defun svn-action-files (&optional pred)
  "Return a list of file names to act on.
Optional argument PRED is a predicate function that is called with POS as
argument."
  (mapcar 'car (svn-actions pred)))

(defun svn-find-file (pos)
  "Find the file under point."
  (interactive "d")
  (let ((filename (or (svn-getprop pos 'file)
                      (svn-getprop pos 'dir))))
    (if filename
        (find-file filename)
      (error "No file on this line"))))

(defun svn-mouse-find-file (ev)
  "Find the file clicked on."
  (interactive "e")
  (svn-find-file (posn-point (event-start ev))))

(defun svn-find-file-other-window ()
  "Find the file under point."
  (interactive)
  (let ((filename (or (svn-getprop (point) 'file)
                      (svn-getprop (point) 'dir))))
    (if filename
        (find-file-other-window filename)
      (error "No file on this line"))))

(defun svn-add-file ()
  "Add the selected files to version control."
  (interactive)
  (let ((actions (svn-action-files
                  (lambda (pos)
                    (or (eq (svn-file-status pos) ?\?)
                        (error "%s is already under version control"
			       (svn-getprop pos 'file)))))))
    (svn-run 'add actions "Adding files")))

(defun svn-add-filter (proc str)
  "Output filter function for `svn add'."
  ;; This filter is shared with the delete command
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert str)
      (goto-char svn-output-marker)
      (while (looking-at
              ;; What format is this, really?
              "\\([AD]  \\).....  \\(.*\\)\n")
        (let ((status (concat (match-string 1) "   "))
              (filename (svn-normalise-path (match-string 2))))
          (delete-region (match-beginning 0)
                         (match-end 0))
          (svn-insert-file filename status))))))

(defun svn-can-undo-deletion-p (actions)
  "Whether all marked files/directories can be deleted undoably"
  (or (null actions)
      (and (let* ((fp (car actions))
		  (pos (cadr fp)))
	     ;; We can safely remove unmodified files under version control,
	     ;; and idempotently already deleted files.
	     (memq (svn-file-status pos) '(?\  ?D)))
	   (svn-can-undo-deletion-p (cdr actions)))))

(defun svn-plural (count noun)
  (format "%d %s" count
	  (if (= count 1)
	      noun
	    (if (equal (substring noun -1) "y")
		(concat (substring noun 0 -1) "ies")
	      (concat noun "s")))))

(defun svn-delete-dir-tree (file)
  "Remove a file or directory tree."
  (cond ((file-symlink-p file)
	 ;; In Emacs 21, delete-file refuses to delete a symlink to a
	 ;; directory. We work around it by overwriting the symlink
	 ;; with a dangling link first. (We can't do that in later
	 ;; Emacs versions, because make-symbolic-link may decide to
	 ;; create the link inside the old link target directory.)
	 (when (<= emacs-major-version 21)
	   (make-symbolic-link "/a/file/that/does/not/exist" file t))
	 (delete-file file))

	((file-directory-p file)
	 (mapc #'(lambda (f)
		   (unless (or (equal f ".") (equal f ".."))
		     (svn-delete-dir-tree (concat file "/" f))))
	       (directory-files file))
	 (delete-directory file))

	(t 				; regular file
	 (delete-file file))))

(defun svn-remove-file ()
  "Remove the selected files and directories."
  (interactive)
  (let* ((actions (svn-actions))
	 (dir-count
	  (length (delq nil (mapcar (lambda (fp)
				      (file-directory-p (car fp)))
				    actions))))
	 (nondir-count (- (length actions) dir-count))
	 (inhibit-read-only t))
    (when (or (svn-can-undo-deletion-p actions)
	      (y-or-n-p
	       (format "Really remove %s? "
		       (cond ((zerop dir-count)
			      (svn-plural nondir-count "file"))
			     ((zerop nondir-count)
			      (svn-plural dir-count "directory"))
			     (t
			      (concat 
			       (svn-plural dir-count "directory")
			       " and "
			       (svn-plural nondir-count "file")))))))
      (let ((svn-files ()))
        (mapc (lambda (fp)
		(let ((file (car fp))
		      (pos (cadr fp)))
		  (if (/= (svn-file-status pos) ?\?)
		      (setq svn-files (cons file svn-files))
		    (svn-delete-dir-tree file)
		    (svn-remove-line pos))))
	      ;; traverse the list backwards, to keep buffer positions of
	      ;; remaining files valid
	      (reverse actions))
        (when svn-files
          (svn-run 'delete (cons "--force" svn-files) "Removing files"))))))

(defun svn-delete-filter (proc str)
  (svn-add-filter proc str))

(defun svn-revert ()
  "Revert the selected files."
  (interactive)
  (let ((files (svn-action-files
                (lambda (pos)
                  (or (memq (svn-file-status pos) '(?D ?A ?M ?C ?!))
                      (memq (svn-prop-status pos) '(?D ?A ?M ?C ?!))
                      (error "%s has no local changes"
			     (svn-getprop pos 'file)))))))
    (when (y-or-n-p (format "Really revert %d %s? "
                            (length files)
                            (if (> (length files) 1)
                                "files"
                              "file")))
      (make-local-variable 'svn-reverted-files)
      (setq svn-reverted-files files)
      (svn-run 'revert files "Reverting files"))))

(defun svn-revert-sentinel (proc reason)
  (svn-default-sentinel proc reason)
  (if (= (process-exit-status proc) 0)
      (with-current-buffer (process-buffer proc)
        (let ((inhibit-read-only t))
          (svn-run-status-v svn-reverted-files nil))))
  (mapc #'svn-revert-if-needed svn-reverted-files))

(defun svn-resolve ()
  "Mark the selected files as resolved."
  (interactive)
  (let ((files (svn-action-files
                (lambda (pos)
                  (or (memq (svn-file-status pos) '(?C ?!))
		      (memq (svn-prop-status pos) '(?C ?!))
		      (error "%s has no conflicts"
			     (svn-getprop pos 'file)))))))
    (make-local-variable 'svn-resolved-files)
    (setq svn-resolved-files files)
    (svn-run 'resolved files "Marking resolved files")))

(defun svn-resolved-sentinel (proc reason)
  (svn-default-sentinel proc reason)
  (if (= (process-exit-status proc) 0)
      (with-current-buffer (process-buffer proc)
        (let ((inhibit-read-only t))
          (svn-run-status-v svn-resolved-files nil)))))

(defun svn-move ()
  "Move/rename the selected file."
  (interactive)
  (let ((files (svn-action-files)))
    (if (/= (length files) 1)
        (error "Can only rename one file at a time"))
    (when (file-directory-p (car files))
      (error "Can only move files"))
    (let* ((src (car files))
           (dir (file-name-directory src))
           (dst (completing-read "Move to: "
                                 'svn-complete-url
                                 nil nil
                                 dir
                                 'svn-switch-history)))
      (svn-run 'move (list src dst) "Moving file"))))

(defun svn-move-filter (proc str)
  (save-excursion
    (set-buffer (process-buffer proc))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert str)
      (goto-char svn-output-marker)
      (while (looking-at
              "\\([AD]     \\)    \\(.*\\)\n")
        (let ((status (match-string 1))
              (filename (svn-normalise-path (match-string 2))))
          (if (string= status "A     ")
              (setq status "A  +  "))
          (delete-region (match-beginning 0)
                         (match-end 0))
          (svn-insert-file filename status))))))

(defun svn-toggle-file-mark ()
  "Toggle the mark for the file under point."
  (let ((mark (svn-getprop (point) 'mark)))
    (svn-set-mark (line-beginning-position) (not mark))))

(defun svn-toggle-mark ()
  "Toggle the mark for the file under point, or files in the dir under point."
  (interactive)
  (cond ((svn-getprop (point) 'file)
	 (svn-toggle-file-mark))
	((svn-getprop (point) 'dir)
	 (let ((dir (svn-getprop (point) 'dir))
	       file)
	   (save-excursion
	     (forward-line 1)
	     (setq file (svn-getprop (point) 'file))
	     (while (and file
			 (svn-in-dir-p dir file))
	       (svn-toggle-file-mark)
	       (forward-line 1)
	       (setq file (svn-getprop (point) 'file))))))))

(defun svn-change-mark-forward (mark)
  "Set or clear the mark for the file under point and move to next line."
  (cond ((svn-getprop (point) 'file)
         (svn-set-mark (line-beginning-position) mark)
         (let (pos (svn-next-file-pos))
           (if pos
               (svn-next-file 1)
             (next-line 1))))
        ((svn-getprop (point) 'dir)
         (let ((dir (svn-getprop (point) 'dir))
               file)
           (forward-line 1)
           (setq file (svn-getprop (point) 'file))
           (while (and file 
                       (svn-in-dir-p dir file))
             (svn-set-mark (point) mark)
             (forward-line 1)
             (setq file (svn-getprop (point) 'file)))
           (move-to-column goal-column)))
        (t
         (error "No file on line"))))

(defun svn-mark-forward ()
  "Set the mark for the file under point and move to next line."
  (interactive)
  (svn-change-mark-forward t))

(defun svn-mark-backward ()
  "Set the mark for the file under point and move to next line."
  (interactive)
  (svn-previous-file 1)
  (svn-set-mark (line-beginning-position) t))

(defun svn-unmark-forward ()
  "Unset the mark for the file on the previous line."
  (interactive)
  (svn-change-mark-forward nil))

(defun svn-unmark-backward ()
  "Unset the mark for the file on the previous line."
  (interactive)
  (svn-previous-file 1)
  (svn-set-mark (line-beginning-position) nil))

(defun svn-unmark-all ()
  "Unset the mark for the file on the previous line."
  (interactive)
  (let ((pos-list (mapcar 'cadr (svn-actions))))
    (while pos-list
      (svn-set-mark (car pos-list) nil)
      (setq pos-list (cdr pos-list)))))

(defun svn-diff-file (all)
  "Show diff for the file under point.
If the prefix argument ALL is non-NIL, show diff for all selected
files instead."
  (interactive "P")
  (let ((files (if all
                   (svn-action-files)
                 (list (or (svn-getprop (point) 'file)
                           (svn-getprop (point) 'dir)
                           (error "No file on line"))))))
    (unless (svn-run-with-output "diff" (append svn-diff-args files)
				 'diff-mode)
      (message "No difference found"))))

(defun svn-previous-file (arg)
  "Move to the ARGth previous line containing file information."
  (interactive "p")
  (let ((pos (previous-single-property-change (point) 'svn-file)))
    (if (not pos)
        (error "No previous file"))

    ;; Usually we have just found the beginning of the current line
    (when (string= (get-text-property pos 'svn-file)
                   (svn-getprop (point) 'file))
      (setq pos (previous-single-property-change pos 'svn-file))
      (if (not pos)
          (error "No previous file")))

    ;; Skip lines that don't contain file info
    (when (null (get-text-property pos 'svn-file))
      (setq pos (previous-single-property-change pos 'svn-file))
      (if (not pos)
          (error "No previous file")))

    (goto-char (+ pos goal-column))
    (if (> arg 1)
        (svn-previous-file (1- arg)))))

(defun svn-next-file-pos ()
  (let ((pos (next-single-property-change (point) 'svn-file)))
    (and pos
	 ;; Skip lines that don't contain file info
	 (if (null (get-text-property pos 'svn-file))
	     (next-single-property-change pos 'svn-file)
	   pos))))

(defun svn-next-file (arg)
  "Move to the ARGth next line containing file information."
  (interactive "p")
  (let ((pos (svn-next-file-pos)))
    (if pos
        (goto-char (+ pos goal-column))
      ;; Allow stepping past last file
      (if (< (point) svn-files-stop)
          (next-line 1)
        (error "No next file")))

    (if (> arg 1)
        (svn-next-file (1- arg)))))

(defun svn-expunge ()
  "Remove entried for unmodified files."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t)
          (last-dir "/"))
      (goto-char svn-files-stop)
      (forward-line -1)
      (while (>= (point) svn-files-start)
        (let ((dir (svn-getprop (point) 'dir)))
          (if dir
              (progn
                (when last-dir
                  ;; If this is a superdirectory, leave it
                  (unless (and (> (length dir) (length last-dir))
                               (string= (substring dir 0 (length last-dir))
                                        last-dir))
                    (svn-remove-line (point))))
                (setq last-dir dir))
            (let ((file-status (svn-file-status (point)))
                  (prop-status (svn-prop-status (point))))
              (if (and (memq file-status '(?\  ?I ?X))
                       (eq prop-status ?\ ))
                  (svn-remove-line (point))
                (setq last-dir nil)))))
        (forward-line -1))))
  (move-to-column goal-column)
  (setq svn-last-inserted-filename nil))


(defun svn-format-help-column (table)
  (mapcar (lambda (cmd-desc)
	       (let ((cmd (car cmd-desc))
		     (desc (cadr cmd-desc)))
		 (format "%-4s %s"
			 (key-description (car (where-is-internal cmd)))
			 desc)))
	     table))

(defun svn-merge-columns-list (columns fmt)
  (let ((first-lines (mapcar #'car columns)))
    (and (eval `(or ,@first-lines))
	 (cons (mapconcat (lambda (str) (format fmt (or str "")))
			  first-lines " | ")
	       (svn-merge-columns-list (mapcar #'cdr columns) fmt)))))

(defun svn-merge-columns (columns width)
  (mapconcat #'identity
	     (svn-merge-columns-list columns (format "%%-%ds" width))
	     "\n"))

(defun svn-status-help ()
  "Display keyboard help for the svn-status buffer."
  (interactive)
  (message (svn-merge-columns
	    (list (svn-format-help-column
		   '((svn-commit "commit marked files")
		     (svn-add-file "add marked files")
		     (svn-remove-file "remove marked files")
		     (svn-revert "revert marked files")
		     (svn-update-current "update working copy")
		     (svn-resolve "resolve conflicts")
		     (svn-move "rename/move files")
		     (svn-switch "switch working tree")
		     (svn-merge "merge into WC")
		     (svn-propedit "edit properties")))
		  (svn-format-help-column
		   '((svn-mark-forward "mark and go down")
		     (svn-unmark-backward "go up and unmark")
		     (svn-unmark-forward  "unmark and go down")
		     (svn-toggle-mark "toggle mark")
		     (svn-unmark-all "unmark all")))
		  (svn-format-help-column
		   '((svn-find-file "visit file")
		     (svn-find-file-other-window "visit file other win")
		     (svn-diff-file "show file diff")
		     (svn-file-log "show file log")
		     (svn-refresh "refresh all files")
		     (svn-refresh-file "refresh marked files")
		     (svn-refresh-one "refresh named file")
		     (svn-expunge "expunge unchanged"))))
	    24)))

;;; Hooks

(defun svn-buffer-list ()
  "Return a list of svn status buffers."
  (let ((buffers ())
        (all-buffers (buffer-list)))
    (while all-buffers
      (let ((buf (car all-buffers)))
        (if (eq (with-current-buffer buf major-mode)
                'svn-status-mode)
            (setq buffers (cons buf buffers))))
      (setq all-buffers (cdr all-buffers)))
    buffers))

(defun svn-update-status-flag (pos flag &optional prop-flag)
  "Update the status flag column for file at position POS.
Argument FLAG is the character to use."
  (save-excursion
    (goto-char (+ pos svn-status-flags-col))
    (insert-and-inherit flag)
    (delete-char 1)
    (when prop-flag
      (insert-and-inherit prop-flag)
      (delete-char 1))))

(defun svn-update-status-msg (pos msg)
  "Update the message column for file at position POS.
Argument MSG is the character to use."
  (save-excursion
    (goto-char (+ pos svn-status-msg-col))
    (delete-char 9)
    (insert-and-inherit (format "%-9s" msg))))

(defun svn-update-conflict-msg (pos msg)
  (save-excursion
    (let ((filename (svn-getprop pos 'file)))
      (goto-char (+ pos svn-status-file-col (length filename)))
      (delete-char (- (line-end-position) (point)))
      (insert-and-inherit (concat " -- " msg)))))

(defun svn-foreach-svn-buffer (file-name function)
  "Call FUNCTION for each svn status buffer that contains FILE-NAME.
The current buffer will be the svn status buffer, and the arguments to
the function is the local form of the filename and the buffer position
where the file information is."
  (let* ((svn-buffers (svn-buffer-list))
         (inhibit-read-only t)
	 (file-path (file-truename file-name)))
    (while svn-buffers
      (with-current-buffer (car svn-buffers)
        (let ((dir (file-truename default-directory)))
          (when (and (>= (length file-path) (length dir))
                     (string= dir (substring file-path 0 (length dir))))
            (let* ((local-file-name (substring file-path (length dir)))
                   (file-pos (svn-file-pos local-file-name)))
              (funcall function local-file-name file-pos)))))
      (setq svn-buffers (cdr svn-buffers)))))

(defun svn-after-save ()
  "Update svn status buffer when saving a file."
  (svn-foreach-svn-buffer
   (buffer-file-name)
   (lambda (local-file-name file-pos)
     (if file-pos
         (let ((old-status (svn-file-status file-pos)))
           (when (= old-status ?\ )
             (svn-update-status-flag file-pos ?M))
           (svn-update-status-msg file-pos ""))
       (svn-run-status-v (list local-file-name) nil))))
  nil)

(add-hook 'after-save-hook 'svn-after-save)

(defun svn-after-commit ()
  "Update svn status buffer when committing a file from `vc-mode'."
  (svn-foreach-svn-buffer
   (buffer-file-name)
   (lambda (local-file-name file-pos)
     (if file-pos
         (progn
           (svn-update-status-flag file-pos ?\ )
           (svn-update-status-msg file-pos "Committed"))
       (svn-insert-file local-file-name "      " "Committed"))))
  nil)

(add-hook 'vc-checkin-hook 'svn-after-commit)

(defun svn-after-vc-command (command file-or-files flags)
  (when (and (string= command "svn")
             ;; Ignore command that do not modify file
             (not (member (car flags) '("ann" "annotate" "blame"
                                        "diff" "praise" "status"))))
    (mapc (lambda (file)
	    (svn-foreach-svn-buffer
	     file
	     (lambda (local-file-name file-pos)
	       (svn-refresh-item local-file-name t))))
	  ;; In emacs versions prior to 23, the argument is a single file.
	  (if (listp file-or-files)
	      file-or-files
	    (list file-or-files)))))

(add-hook 'vc-post-command-functions 'svn-after-vc-command)

(setq uniquify-list-buffers-directory-modes
      (cons 'svn-status-mode
            uniquify-list-buffers-directory-modes))

(provide 'dsvn)

;;; dsvn.el ends here
