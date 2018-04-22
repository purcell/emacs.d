;;; haskell-process.el --- Communicating with the inferior Haskell process -*- lexical-binding: t -*-

;; Copyright (C) 2011  Chris Done

;; Author: Chris Done <chrisdone@gmail.com>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url-util)
(require 'haskell-compat)
(require 'haskell-session)
(require 'haskell-customize)
(require 'haskell-string)

(defconst haskell-process-prompt-regex "\4"
  "Used for delimiting command replies. 4 is End of Transmission.")

(defvar haskell-reload-p nil
  "Used internally for `haskell-process-loadish'.")

(defconst haskell-process-greetings
  (list "Hello, Haskell!"
        "The lambdas must flow."
        "Hours of hacking await!"
        "The next big Haskell project is about to start!"
        "Your wish is my IO ().")
  "Greetings for when the Haskell process starts up.")

(defconst haskell-process-logo
  (expand-file-name "logo.svg" haskell-mode-pkg-base-dir)
  "Haskell logo for notifications.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing commands -- using cl 'defstruct'

(cl-defstruct haskell-command
  "Data structure representing a command to be executed when with
  a custom state and three callback."
  ;; hold the custom command state
  ;; state :: a
  state
  ;; called when to execute a command
  ;; go :: a -> ()
  go
  ;; called whenever output was collected from the haskell process
  ;; live :: a -> Response -> Bool
  live
  ;; called when the output from the haskell process indicates that the command
  ;; is complete
  ;; complete :: a -> Response -> ()
  complete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the process

(defun haskell-process-compute-process-log-and-command (session hptype)
  "Compute the log and process to start command for the SESSION from the HPTYPE.
Do not actually start any process.
HPTYPE is the result of calling `'haskell-process-type`' function."
  (let ((session-name (haskell-session-name session)))
    (cl-ecase hptype
      ('ghci
       (append (list (format "Starting inferior GHCi process %s ..."
                             haskell-process-path-ghci)
                     session-name
                     nil)
               (apply haskell-process-wrapper-function
                      (list
                       (append (haskell-process-path-to-list haskell-process-path-ghci)
                               haskell-process-args-ghci)))))
      ('cabal-new-repl
       (append (list (format "Starting inferior `cabal new-repl' process using %s ..."
                             haskell-process-path-cabal)
                     session-name
                     nil)
               (apply haskell-process-wrapper-function
                      (list
                       (append
                        (haskell-process-path-to-list haskell-process-path-cabal)
                        (list "new-repl")
                        haskell-process-args-cabal-new-repl
                        (let ((target (haskell-session-target session)))
                          (if target (list target) nil)))))))
      ('cabal-repl
       (append (list (format "Starting inferior `cabal repl' process using %s ..."
                             haskell-process-path-cabal)
                     session-name
                     nil)
               (apply haskell-process-wrapper-function
                      (list
                       (append
                        (haskell-process-path-to-list haskell-process-path-cabal)
                        (list "repl")
                        haskell-process-args-cabal-repl
                        (let ((target (haskell-session-target session)))
                          (if target (list target) nil)))))))
      ('stack-ghci
       (append (list (format "Starting inferior stack GHCi process using %s" haskell-process-path-stack)
                     session-name
                     nil)
               (apply haskell-process-wrapper-function
                      (list
                       (append
                        (haskell-process-path-to-list haskell-process-path-stack)
                        (list "ghci")
                        (let ((target (haskell-session-target session)))
                          (if target (list target) nil))
                        haskell-process-args-stack-ghci))))))))

(defun haskell-process-path-to-list (path)
  "Convert a path (which may be a string or a list) to a list."
  (if (stringp path)
      (list path)
    path))

(defun haskell-process-make (name)
  "Make an inferior Haskell process."
  (list (cons 'name name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process communication

(defun haskell-process-sentinel (proc event)
  "The sentinel for the process pipe."
  (let ((session (haskell-process-project-by-proc proc)))
    (when session
      (let* ((process (haskell-session-process session)))
        (unless (haskell-process-restarting process)
          (haskell-process-log
           (propertize (format "Event: %S\n" event)
                       'face '((:weight bold))))
          (haskell-process-log
           (propertize "Process reset.\n"
                       'face 'font-lock-comment-face))
          (run-hook-with-args 'haskell-process-ended-functions process))))))

(defun haskell-process-filter (proc response)
  "The filter for the process pipe."
  (let ((i 0))
    (cl-loop for line in (split-string response "\n")
             do (haskell-process-log
                 (concat (if (= i 0)
                             (propertize "<- " 'face 'font-lock-comment-face)
                           "   ")
                         (propertize line 'face 'haskell-interactive-face-compile-warning)))
             do (setq i (1+ i))))
  (let ((session (haskell-process-project-by-proc proc)))
    (when session
      (if (haskell-process-cmd (haskell-session-process session))
          (haskell-process-collect session
                                   response
                                   (haskell-session-process session))))))

(defun haskell-process-log (msg)
  "Effective append MSG to the process log (if enabled)."
  (when haskell-process-log
    (let* ((append-to (get-buffer-create "*haskell-process-log*")))
      (with-current-buffer append-to
        ;; point should follow insertion so that it stays at the end
        ;; of the buffer
        (setq-local window-point-insertion-type t)
        (let ((buffer-read-only nil))
          (insert msg "\n"))))))

(defun haskell-process-project-by-proc (proc)
  "Find project by process."
  (cl-find-if (lambda (project)
                (string= (haskell-session-name project)
                         (process-name proc)))
              haskell-sessions))

(defun haskell-process-collect (_session response process)
  "Collect input for the response until receives a prompt."
  (haskell-process-set-response process
                                (concat (haskell-process-response process) response))
  (while (haskell-process-live-updates process))
  (when (string-match haskell-process-prompt-regex
                      (haskell-process-response process))
    (haskell-command-exec-complete
     (haskell-process-cmd process)
     (replace-regexp-in-string
      haskell-process-prompt-regex
      ""
      (haskell-process-response process)))
    (haskell-process-reset process)
    (haskell-process-trigger-queue process)))

(defun haskell-process-reset (process)
  "Reset the process's state, ready for the next send/reply."
  (progn (haskell-process-set-response-cursor process 0)
         (haskell-process-set-response process "")
         (haskell-process-set-cmd process nil)))

(defun haskell-process-consume (process regex)
  "Consume a regex from the response and move the cursor along if succeed."
  (when (string-match regex
                      (haskell-process-response process)
                      (haskell-process-response-cursor process))
    (haskell-process-set-response-cursor process (match-end 0))
    t))

(defun haskell-process-send-string (process string)
  "Try to send a string to the process's process. Ask to restart if it's not running."
  (let ((child (haskell-process-process process)))
    (if (equal 'run (process-status child))
        (let ((out (concat string "\n")))
          (let ((i 0))
            (cl-loop for line in (split-string out "\n")
                     do (unless (string-equal "" line)
                          (haskell-process-log
                           (concat (if (= i 0)
                                       (propertize "-> " 'face 'font-lock-comment-face)
                                     "   ")
                                   (propertize line 'face 'font-lock-string-face))))
                     do (setq i (1+ i))))
          (process-send-string child out))
      (unless (haskell-process-restarting process)
        (run-hook-with-args 'haskell-process-ended-functions process)))))

(defun haskell-process-live-updates (process)
  "Process live updates."
  (haskell-command-exec-live (haskell-process-cmd process)
                             (haskell-process-response process)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making commands

(defun haskell-process-queue-without-filters (process line)
  "Queue LINE to be sent to PROCESS without bothering to look at
the response."
  (haskell-process-queue-command
   process
   (make-haskell-command
    :state (cons process line)
    :go (lambda (state)
          (haskell-process-send-string (car state)
                                       (cdr state))))))


(defun haskell-process-queue-command (process command)
  "Add a command to the process command queue."
  (haskell-process-cmd-queue-add process command)
  (haskell-process-trigger-queue process))

(defun haskell-process-trigger-queue (process)
  "Trigger the next command in the queue to be ran if there is no current command."
  (if (and (haskell-process-process process)
           (process-live-p (haskell-process-process process)))
      (unless (haskell-process-cmd process)
        (let ((cmd (haskell-process-cmd-queue-pop process)))
          (when cmd
            (haskell-process-set-cmd process cmd)
            (haskell-command-exec-go cmd))))
    (progn (haskell-process-reset process)
           (haskell-process-set process 'command-queue nil)
           (run-hook-with-args 'haskell-process-ended-functions process))))

(defun haskell-process-queue-flushed-p (process)
  "Return t if command queue has been completely processed."
  (not (or (haskell-process-cmd-queue process)
           (haskell-process-cmd process))))

(defun haskell-process-queue-flush (process)
  "Block till PROCESS' command queue has been completely processed.
This uses `accept-process-output' internally."
  (while (not (haskell-process-queue-flushed-p process))
    (haskell-process-trigger-queue process)
    (accept-process-output (haskell-process-process process) 1)))

(defun haskell-process-queue-sync-request (process reqstr)
  "Queue submitting REQSTR to PROCESS and return response blockingly."
  (let ((cmd (make-haskell-command
              :state (cons nil process)
              :go `(lambda (s) (haskell-process-send-string (cdr s) ,reqstr))
              :complete 'setcar)))
    (haskell-process-queue-command process cmd)
    (haskell-process-queue-flush process)
    (car-safe (haskell-command-state cmd))))

(defun haskell-process-get-repl-completions (process inputstr &optional limit)
  "Query PROCESS with `:complete repl ...' for INPUTSTR.
Give optional LIMIT arg to limit completion candidates count,
zero, negative values, and nil means all possible completions.
Returns NIL when no completions found."
  (let* ((mlimit (if (and limit (> limit 0))
                     (concat " " (number-to-string limit) " ")
                   " "))
         (reqstr (concat ":complete repl"
                         mlimit
                         (haskell-string-literal-encode inputstr)))
         (rawstr (haskell-process-queue-sync-request process reqstr))
         (response-status (haskell-utils-repl-response-error-status rawstr)))
    (if (eq 'unknown-command response-status)
        (error
         "GHCi lacks `:complete' support (try installing GHC 7.8+ or ghci-ng)")
      (when rawstr
        ;; parse REPL response if any
        (let* ((s1 (split-string rawstr "\r?\n" t))
               (cs (mapcar #'haskell-string-literal-decode (cdr s1)))
               (h0 (car s1))) ;; "<limit count> <all count> <unused string>"
          (unless (string-match
                   "\\`\\([0-9]+\\) \\([0-9]+\\) \\(\".*\"\\)\\'"
                   h0)
            (error "Invalid `:complete' response"))
          (let ((cnt1 (match-string 1 h0))
                (h1 (haskell-string-literal-decode (match-string 3 h0))))
            (unless (= (string-to-number cnt1) (length cs))
              (error "Lengths inconsistent in `:complete' reponse"))
            (cons h1 cs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessing the process

(defun haskell-process-get (process key)
  "Get the PROCESS's KEY value.
Returns nil if KEY not set."
  (cdr (assq key process)))

(defun haskell-process-set (process key value)
  "Set the PROCESS's KEY to VALUE.
Returns newly set VALUE."
  (if process
      (let ((cell (assq key process)))
        (if cell
            (setcdr cell value)         ; modify cell in-place
          (setcdr process (cons (cons key value) (cdr process))) ; new cell
          value))
    (display-warning 'haskell-interactive
                     "`haskell-process-set' called with nil process")))

;; Wrappers using haskell-process-{get,set}

(defun haskell-process-set-sent-stdin (p v)
  "We've sent stdin, so let's not clear the output at the end."
  (haskell-process-set p 'sent-stdin v))

(defun haskell-process-sent-stdin-p (p)
  "Did we send any stdin to the process during evaluation?"
  (haskell-process-get p 'sent-stdin))

(defun haskell-process-set-suggested-imports (p v)
  "Remember what imports have been suggested, to avoid
re-asking about the same imports."
  (haskell-process-set p 'suggested-imported v))

(defun haskell-process-suggested-imports (p)
  "Get what modules have already been suggested and accepted."
  (haskell-process-get p 'suggested-imported))

(defun haskell-process-set-evaluating (p v)
  "Set status of evaluating to be on/off."
  (haskell-process-set p 'evaluating v))

(defun haskell-process-evaluating-p (p)
  "Get status of evaluating (on/off)."
  (haskell-process-get p 'evaluating))

(defun haskell-process-set-process (p v)
  "Set the process's inferior process."
  (haskell-process-set p 'inferior-process v))

(defun haskell-process-process (p)
  "Get the process child."
  (haskell-process-get p 'inferior-process))

(defun haskell-process-name (p)
  "Get the process name."
  (haskell-process-get p 'name))

(defun haskell-process-cmd (p)
  "Get the process's current command.
Return nil if no current command."
  (haskell-process-get p 'current-command))

(defun haskell-process-set-cmd (p v)
  "Set the process's current command."
  (haskell-process-set-evaluating p nil)
  (haskell-process-set-sent-stdin p nil)
  (haskell-process-set-suggested-imports p nil)
  (haskell-process-set p 'current-command v))

(defun haskell-process-response (p)
  "Get the process's current response."
  (haskell-process-get p 'current-response))

(defun haskell-process-session (p)
  "Get the process's current session."
  (haskell-process-get p 'session))

(defun haskell-process-set-response (p v)
  "Set the process's current response."
  (haskell-process-set p 'current-response v))

(defun haskell-process-set-session (p v)
  "Set the process's current session."
  (haskell-process-set p 'session v))

(defun haskell-process-response-cursor (p)
  "Get the process's current response cursor."
  (haskell-process-get p 'current-response-cursor))

(defun haskell-process-set-response-cursor (p v)
  "Set the process's response cursor."
  (haskell-process-set p 'current-response-cursor v))

;; low-level command queue operations

(defun haskell-process-restarting (process)
  "Is the PROCESS restarting?"
  (haskell-process-get process 'is-restarting))

(defun haskell-process-cmd-queue (process)
  "Get the PROCESS' command queue.
New entries get added to the end of the list. Use
`haskell-process-cmd-queue-add' and
`haskell-process-cmd-queue-pop' to modify the command queue."
  (haskell-process-get process 'command-queue))

(defun haskell-process-cmd-queue-add (process cmd)
  "Add CMD to end of PROCESS's command queue."
  (cl-check-type cmd haskell-command)
  (haskell-process-set process
                       'command-queue
                       (append (haskell-process-cmd-queue process)
                               (list cmd))))

(defun haskell-process-cmd-queue-pop (process)
  "Pop the PROCESS' next entry from command queue.
Returns nil if queue is empty."
  (let ((queue (haskell-process-cmd-queue process)))
    (when queue
      (haskell-process-set process 'command-queue (cdr queue))
      (car queue))))


(defun haskell-process-unignore-file (session file)
  "

Note to Windows Emacs hackers:

chmod is how to change the mode of files in POSIX
systems. This will not work on your operating
system.

There is a command a bit like chmod called \"Calcs\"
that you can try using here:

http://technet.microsoft.com/en-us/library/bb490872.aspx

If it works, you can submit a patch to this
function and remove this comment.
"
  (shell-command (read-from-minibuffer "Permissions command: "
                                       (concat "chmod 700 "
                                               file)))
  (haskell-session-modify
   session
   'ignored-files
   (lambda (files)
     (cl-remove-if (lambda (path)
                     (string= path file))
                   files))))

(defun haskell-command-exec-go (command)
  "Call the command's go function."
  (let ((go-func (haskell-command-go command)))
    (when go-func
      (funcall go-func (haskell-command-state command)))))

(defun haskell-command-exec-complete (command response)
  "Call the command's complete function."
  (let ((comp-func (haskell-command-complete command)))
    (when comp-func
      (condition-case-unless-debug e
          (funcall comp-func
                   (haskell-command-state command)
                   response)
        (quit (message "Quit"))
        (error (message "Haskell process command errored with: %S" e))))))

(defun haskell-command-exec-live (command response)
  "Trigger the command's live updates callback."
  (let ((live-func (haskell-command-live command)))
    (when live-func
      (funcall live-func
               (haskell-command-state command)
               response))))

(provide 'haskell-process)

;;; haskell-process.el ends here
