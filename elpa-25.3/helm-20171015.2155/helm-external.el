;;; helm-external.el --- Run Externals commands within Emacs with helm completion. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2017 Thierry Volpiatto <thierry.volpiatto@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-help)
(require 'helm-net)


(defgroup helm-external nil
  "External related Applications and libraries for Helm." 
  :group 'helm)

(defcustom helm-raise-command nil
  "A shell command to jump to a window running specific program.
Need external program wmctrl.
This will be use with `format', so use something like \"wmctrl -xa %s\"."
  :type 'string
  :group 'helm-external)

(defcustom helm-external-programs-associations nil
  "Alist to store externals programs associated with file extension.
This variable overhide setting in .mailcap file.
e.g : '\(\(\"jpg\" . \"gqview\"\) (\"pdf\" . \"xpdf\"\)\) "
  :type '(alist :key-type string :value-type string)
  :group 'helm-external)

(defcustom helm-default-external-file-browser "nautilus"
  "Default external file browser for your system.
Directories will be opened externally with it when
opening file externally in `helm-find-files'.
Set to nil if you do not have external file browser
or do not want to use it.
Windows users should set that to \"explorer.exe\"."
  :group 'helm-external
  :type  'string)


;;; Internals
(defvar helm-external-command-history nil)
(defvar helm-external-commands-list nil
  "A list of all external commands the user can execute.
If this variable is not set by the user, it will be calculated
automatically.")

(defun helm-external-commands-list-1 (&optional sort)
  "Returns a list of all external commands the user can execute.
If `helm-external-commands-list' is non-nil it will
return its contents.  Else it calculates all external commands
and sets `helm-external-commands-list'."
  (helm-aif helm-external-commands-list
      it
    (setq helm-external-commands-list
          (cl-loop
                for dir in (split-string (getenv "PATH") path-separator)
                when (and (file-exists-p dir) (file-accessible-directory-p dir))
                for lsdir = (cl-loop for i in (directory-files dir t)
                                  for bn = (file-name-nondirectory i)
                                  when (and (not (member bn completions))
                                            (not (file-directory-p i))
                                            (file-executable-p i))
                                  collect bn)
                append lsdir into completions
                finally return
                (if sort (sort completions 'string-lessp) completions)))))

(defun helm-run-or-raise (exe &optional file)
  "Run asynchronously EXE or jump to the application window.
If EXE is already running just jump to his window if `helm-raise-command'
is non--nil.
When FILE argument is provided run EXE with FILE."
  (let* ((real-com (car (split-string exe)))
         (proc     (if file (concat real-com " " file) real-com))
         process-connection-type)
    (if (get-process proc)
        (if helm-raise-command
            (shell-command  (format helm-raise-command real-com))
          (error "Error: %s is already running" real-com))
      (when (member real-com helm-external-commands-list)
        (message "Starting %s..." real-com)
        (if file
            (start-process-shell-command
             proc nil (format "%s %s"
                              real-com
                              (shell-quote-argument
                               (if (eq system-type 'windows-nt)
                                   (helm-w32-prepare-filename file)
                                   (expand-file-name file)))))
          (start-process-shell-command proc nil real-com))
        (set-process-sentinel
         (get-process proc)
         (lambda (process event)
             (when (and (string= event "finished\n")
                        helm-raise-command
                        (not (helm-get-pid-from-process-name real-com)))
               (shell-command  (format helm-raise-command "emacs")))
             (message "%s process...Finished." process))))
      (setq helm-external-commands-list
            (cons real-com
                  (delete real-com helm-external-commands-list))))))

(defun helm-get-mailcap-for-file (filename)
  "Get the command to use for FILENAME from mailcap files."
  (mailcap-parse-mailcaps)
  (let* ((ext  (file-name-extension filename))
         (mime (when ext (mailcap-extension-to-mime ext)))
         (result (when mime (mailcap-mime-info mime))))
    ;; If elisp file have no associations in .mailcap
    ;; `mailcap-maybe-eval' is returned, in this case just return nil.
    (when (stringp result) (helm-basename result))))

(defun helm-get-default-program-for-file (filename)
  "Try to find a default program to open FILENAME.
Try first in `helm-external-programs-associations' and then in mailcap file
if nothing found return nil."
  (let* ((ext      (file-name-extension filename))
         (def-prog (assoc-default ext helm-external-programs-associations)))
    (cond ((and def-prog (not (string= def-prog ""))) def-prog)
          ((and helm-default-external-file-browser (file-directory-p filename))
           helm-default-external-file-browser)
          (t (helm-get-mailcap-for-file filename)))))

(defun helm-open-file-externally (file)
  "Open FILE with an external program.
Try to guess which program to use with `helm-get-default-program-for-file'.
If not found or a prefix arg is given query the user which tool to use."
  (let* ((fname      (expand-file-name file))
         (collection (helm-external-commands-list-1 'sort))
         (def-prog   (helm-get-default-program-for-file fname))
         (program    (if (or helm-current-prefix-arg (not def-prog))
                         ;; Prefix arg or no default program.
                         (prog1
                             (helm-comp-read
                              "Program: " collection
                              :must-match t
                              :name "Open file Externally"
                              :del-input nil
                              :history helm-external-command-history)
                           ;; Always prompt to set this program as default.
                           (setq def-prog nil))
                         ;; No prefix arg or default program exists.
                         def-prog)))
    (unless (or def-prog ; Association exists, no need to record it.
                ;; Don't try to record non--filenames associations (e.g urls).
                (not (file-exists-p fname)))
      (when
          (y-or-n-p
           (format
            "Do you want to make `%s' the default program for this kind of files? "
            program))
        (helm-aif (assoc (file-name-extension fname)
                         helm-external-programs-associations)
            (setq helm-external-programs-associations
                  (delete it helm-external-programs-associations)))
        (push (cons (file-name-extension fname)
                    (helm-read-string
                     "Program (Add args maybe and confirm): " program))
              helm-external-programs-associations)
        (customize-save-variable 'helm-external-programs-associations
                                 helm-external-programs-associations)))
    (helm-run-or-raise program file)
    (setq helm-external-command-history
          (cons program
                (delete program
                        (cl-loop for i in helm-external-command-history
                              when (executable-find i) collect i))))))

;;;###autoload
(defun helm-run-external-command (program)
  "Preconfigured `helm' to run External PROGRAM asyncronously from Emacs.
If program is already running exit with error.
You can set your own list of commands with
`helm-external-commands-list'."
  (interactive (list
                (helm-comp-read
                 "RunProgram: "
                 (helm-external-commands-list-1 'sort)
                 :must-match t
                 :del-input nil
                 :name "External Commands"
                 :history helm-external-command-history)))
  (helm-run-or-raise program)
  (setq helm-external-command-history
        (cons program (delete program
                              (cl-loop for i in helm-external-command-history
                                    when (executable-find i) collect i)))))


(provide 'helm-external)

;; Local Variables:
;; byte-compile-warnings: (not obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; helm-external ends here
