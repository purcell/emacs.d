;;; sunrise-x-loop.el --- asynchronous execution of filesystem operations for the Sunrise Commander File Manager -*- lexical-binding: t -*-

;; Copyright (C) 2008-2012 José Alfredo Romero Latouche.

;; Author: José Alfredo Romero L. <escherdragon@gmail.com>
;;	Štěpán Němec <stepnem@gmail.com>
;; Maintainer: José Alfredo Romero L. <escherdragon@gmail.com>
;; Created: 27 Jun 2008
;; Version: 3
;; RCS Version: $Rev: 423 $
;; Keywords: sunrise commander, background copy rename move
;; URL: http://www.emacswiki.org/emacs/sunrise-x-loop.el
;; Compatibility: GNU Emacs 22+

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more de-
;; tails.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This extension adds to the Sunrise Commander the capability of performing
;; copy and rename operations in the background. It provides prefixable drop-in
;; replacements for the `sr-do-copy' and `sr-do-rename' commands and uses them
;; to redefine their bindings in the `sr-mode-map' keymap. When invoked the
;; usual way (by pressing C or R), these new functions work exactly as the old
;; ones, i.e. they simply pass the control flow to the logic already provided by
;; Sunrise, but when prefixed (e.g. by pressing C-u C or C-u R) they launch a
;; separate Elisp intepreter in the background, delegate to it the execution of
;; all further operations and return immediately, so the Emacs UI remains fully
;; responsive while any potentially long-running copy or move tasks can be let
;; alone to eventually reach their completion in the background.

;; After all requested actions have been performed, the background interpreter
;; remains active for a short period of time (30 seconds by default, but it can
;; be customized), after which it shuts down automatically.

;; At any moment you can abort all tasks scheduled and under execution and force
;; the background interpreter to shut down by invoking the `sr-loop-stop'
;; command (M-x sr-loop-stop).

;; If you need to debug something or are just curious about how this extension
;; works, you can set the variable `sr-loop-debug' to t to have the interpreter
;; launched in debug mode. In this mode all input and output of background
;; operations are sent to a buffer named *SUNRISE-LOOP*. To return to normal
;; mode set `sr-loop-debug' back to nil and use `sr-loop-stop' to kill the
;; currently running interpreter.

;; The extension disables itself and tries to do its best to keep out of the way
;; when working with remote directories through FTP (e.g. when using ange-ftp),
;; since in these cases the execution of file transfers in the background should
;; be managed directly by the FTP client.

;; It was written on GNU Emacs 23 on Linux, and tested on GNU Emacs 22 and 23
;; for Linux and on EmacsW32 (version 22) for Windows.

;;; Installation and Usage:

;; 1) Put this file somewhere in your Emacs `load-path'.

;; 2) Add a (require 'sunrise-x-loop) expression to your .emacs file somewhere
;; after the (require 'sunrise-commander) one.

;; 3) Evaluate the new expression, or reload your .emacs file, or restart Emacs.

;; 4) The next time you need to copy of move any big files, just prefix the
;; appropriate command with C-u.

;; 5) Enjoy ;-)

;; 6) You can use `unload-feature' to get rid of the provided functionality
;; completely.

;;; Code:

(require 'sunrise-commander)

(defcustom sr-loop-debug nil
  "Activate debug mode in the Sunrise Loop extension.
When set, the background elisp interpreter is launched in such a
way that all background input and output are sent to a buffer
named *SUNRISE LOOP* and automatic lifecycle management is
disabled (i.e. you have to kill the interpreter manually using
sr-loop-stop to get rid of it)."
  :group 'sunrise
  :type 'boolean)

(defcustom sr-loop-timeout 30
  "Number of seconds to wait while idle before shutting down the interpreter.
After executing one or more operations in the background, the
Sunrise Loop Elisp interpreter will be killed automatically after
this amount of time."
  :group 'sunrise)

(defcustom sr-loop-use-popups t
  "When non-nil, display pop‐up notification when execution queue is emptied."
  :group 'sunrise
  :type 'boolean)

(defvar sr-loop-process nil)
(defvar sr-loop-timer nil)
(defvar sr-loop-scope nil)
(defvar sr-loop-queue nil)

(defun sr-loop-start ()
  "Launch and initiate a new background Elisp interpreter.
The new interpreter runs in batch mode and inherits all functions
from the Sunrise Commander (sunrise-commander.el) and from this
file."
  (let ((process-connection-type nil)
        (sr-main (symbol-file 'sr-mode))
        (sr-loop (symbol-file 'sr-loop-cmd-loop))
        (emacs (concat invocation-directory invocation-name)))
    (setq sr-loop-process (start-process
                         "Sunrise-Loop"
                         (if sr-loop-debug "*SUNRISE-LOOP*" nil)
                         emacs
                         "-batch" "-q" "-no-site-file"
                         "-l" sr-main "-l" sr-loop
                         "-eval" "(sr-loop-cmd-loop)"))
    (sr-loop-enqueue `(setq load-path (quote ,load-path)))
    (sr-loop-enqueue '(require 'sunrise-commander))
    (if sr-loop-debug
        (sr-loop-enqueue '(setq sr-loop-debug t))
      (set-process-filter sr-loop-process 'sr-loop-filter))
    (setq sr-loop-queue nil)))

(defun sr-loop-disable-timer ()
  "Disable the automatic shutdown timer.
This is done every time we send a new task to the background
interpreter, lest it gets nuked before completing its queue."
  (if sr-loop-timer
      (progn
        (cancel-timer sr-loop-timer)
        (setq sr-loop-timer nil))))

(defun sr-loop-enable-timer ()
  "Enable the automatic shutdown timer.
This is done every time we receive confirmation from the
background interpreter that all the tasks delegated to it have
been completed. Once this function is executed, if no new tasks
are enqueued before `sr-loop-timeout' seconds, the interpreter is
killed."
  (sr-loop-disable-timer)
  (setq sr-loop-timer (run-with-timer sr-loop-timeout nil 'sr-loop-stop)))

(defun sr-loop-stop (&optional interrupt)
  "Shut down the background Elisp interpreter and clean up after it."
  (interactive "p")
  (sr-loop-disable-timer)
  (if sr-loop-queue
      (if interrupt
          (progn
            (sr-loop-notify "Aborted. Some operations may remain unfinished.")
            (setq sr-loop-queue nil))
        (sr-loop-enable-timer)))
  (unless sr-loop-queue
    (delete-process sr-loop-process)
    (setq sr-loop-process nil)))

(defun sr-loop-notify (msg)
  "Notify the user about an event."
  (if (and window-system sr-loop-use-popups)
      (x-popup-dialog t (list msg '("OK")) t)
    (message (concat "[[" msg "]]"))))

(defun sr-loop-filter (_process output)
  "Process filter for the background interpreter."
  (mapc (lambda (line)
          (cond ((string-match "^\\[\\[\\*\\([^\]\*]+\\)\\*\\]\\]$" line)
                 (sr-loop-notify (match-string 1 line)))

                ((and (or (string-match "^\\[\\[" line)
                          (string-match "^Sunrise Loop: " line))
                      (< 0 (length line)))
                 (message "%s" line))

                ((eq ?^ (string-to-char line))
                 (let ((command (substring line 1)))
                   (when (string= command (car sr-loop-queue))
                     (pop sr-loop-queue)
                     (sr-loop-enable-timer)
                     (unless sr-loop-queue
                       (sr-loop-notify "Background job finished!")))))
                (t nil)))
        (split-string output "\n")))

(defun sr-loop-enqueue (form)
  "Delegate evaluation of FORM to the background interpreter.
If no such interpreter is currently running, launches a new one."
  (sr-loop-disable-timer)
  (unless sr-loop-process
    (sr-loop-start))
  (let ((command (prin1-to-string form)))
    (setq sr-loop-queue (append sr-loop-queue (list (md5 command))))
    (process-send-string sr-loop-process command)
    (process-send-string sr-loop-process "\n")))

(defun sr-loop-cmd-loop ()
  "Main execution loop for the background Elisp interpreter."
  (sr-ad-disable "^sr-loop-")
  (defun read-char nil ?y) ;; Always answer "yes" to any prompt
  (let ((command) (signature))
    (while t
      (setq command (read))
      (setq signature (md5 (prin1-to-string command)))
      (condition-case description
          (progn
            (if sr-loop-debug
                (message "%s" (concat "[[Executing in background: "
                                      (prin1-to-string command) "]]")))
            (eval command)
            (message "[[Command successfully invoked in background]]"))
        (error (message "%s" (concat "[[*ERROR IN BACKGROUND JOB: "
                                     (prin1-to-string description) "*]]"))))
        (message "^%s" signature))))

(defun sr-loop-applicable-p ()
  "Return non-nil if an operation is suitable for the background interpreter."
  (and (null (string-match "^/ftp:" dired-directory))
       (null (string-match "^/ftp:" sr-other-directory))))

(defun sr-loop-do-copy (&optional arg)
  "Drop-in prefixable replacement for the `sr-do-copy' command.
When invoked with a prefix argument, sets a flag that is used
later by advice to decide whether to delegate further copy
operations to the background interpreter."
  (interactive "P")
  (if (and arg (sr-loop-applicable-p))
      (let ((sr-loop-scope t))
        (sr-do-copy))
    (sr-do-copy)))

(defun sr-loop-do-clone (&optional arg)
  "Drop-in prefixable replacement for the `sr-do-clone' command.
When invoked with a prefix argument, sets a flag that is used
later by advice to decide whether to delegate further copy
operations to the background interpreter."
  (interactive "P")
  (if (and arg (sr-loop-applicable-p))
      (let ((sr-loop-scope t))
        (call-interactively 'sr-do-clone))
    (call-interactively 'sr-do-clone)))

(defun sr-loop-do-rename (&optional arg)
  "Drop-in  prefixable  replacement  for  the `sr-do-rename' command.
When invoked with a prefix argument, sets a flag that is used
later by advice to decide whether to delegate further rename
operations to the background interpreter."
  (interactive "P")
  (if (and arg (sr-loop-applicable-p))
      (let ((sr-loop-scope t))
        (sr-do-rename))
    (sr-do-rename)))

(defadvice sr-progress-prompt (around sr-loop-advice-sr-progress-prompt
                                      activate)
  "Display \"Sunrise Loop\" instead of \"Sunrise\" in the prompt."
  (setq ad-return-value
        (concat (if sr-loop-scope "Sunrise Loop: " "Sunrise: ")
                (ad-get-arg 0)
                "...")))

(defadvice y-or-n-p (before sr-loop-advice-y-or-n-p activate)
  "Modify all confirmation request messages inside a loop scope."
  (when sr-loop-scope
    (setq (ad-get-arg 0)
          (replace-regexp-in-string
           "\?" " in the background? (overwrites ALWAYS!)" (ad-get-arg 0)))))

(defadvice dired-mark-read-file-name
  (before sr-loop-advice-dired-mark-read-file-name
          (prompt dir op-symbol arg files &optional default)
          activate)
  "Modify all queries from Dired inside a loop scope."
  (if sr-loop-scope
      (setq prompt (replace-regexp-in-string
                    "^\\([^ ]+\\) ?\\(.*\\)"
                    "\\1 (in background - overwrites ALWAYS!) \\2" prompt))))

(defadvice dired-create-files
  (around sr-loop-advice-dired-create-files
          (file-creator operation fn-list name-constructor
                        &optional marker-char)
          activate)
  "Delegate to the background interpreter all copy and rename operations
triggered by `dired-do-copy' inside a loop scope."
  (if sr-loop-scope
      (with-no-warnings
        (sr-loop-enqueue
         `(let ((target ,target))       ; cf. `dired-do-create-files'
            (dired-create-files (function ,file-creator)
                                ,operation
                                (quote ,fn-list)
                                ,name-constructor nil))))
    ad-do-it))

(defadvice sr-clone-files
  (around sr-loop-advice-sr-clone-files
          (file-path-list target-dir clone-op progress &optional do-overwrite)
          activate)
  "Delegate to the background interpreter all copy operations
triggered by `sr-do-copy' inside a loop scope."
  (if sr-loop-scope
      (sr-loop-enqueue
       `(sr-clone-files
         (quote ,file-path-list) ,target-dir #',clone-op ',progress 'ALWAYS))
    ad-do-it))

(defadvice sr-move-files
  (around sr-loop-advice-sr-move-files
          (file-path-list target-dir progress &optional do-overwrite)
          activate)
  "Delegate to the background interpreter all rename operations
triggered by `sr-do-rename' inside a loop scope."
  (if sr-loop-scope
      (sr-loop-enqueue
       `(sr-move-files (quote ,file-path-list) ,target-dir ',progress 'ALWAYS))
    ad-do-it))

(define-key sr-mode-map "C" 'sr-loop-do-copy)
(define-key sr-mode-map "K" 'sr-loop-do-clone)
(define-key sr-mode-map "R" 'sr-loop-do-rename)

(defun sunrise-x-loop-unload-function ()
  (sr-ad-disable "^sr-loop-")
  (define-key sr-mode-map "C" 'sr-do-copy)
  (define-key sr-mode-map "K" 'sr-do-clone)
  (define-key sr-mode-map "R" 'sr-do-rename))

(provide 'sunrise-x-loop)

;;;###autoload (eval-after-load 'sunrise-commander '(sr-extend-with 'sunrise-x-loop))

;;; sunrise-x-loop.el ends here
