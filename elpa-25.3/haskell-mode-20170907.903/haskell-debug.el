;;; haskell-debug.el --- Debugging mode via GHCi -*- lexical-binding: t -*-

;; Copyright © 2014 Chris Done. All rights reserved.
;;             2016 Arthur Fayzrakhmanov

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'haskell-session)
(require 'haskell-process)
(require 'haskell-interactive-mode)
(require 'haskell-font-lock)
(require 'haskell-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration

;;;###autoload
(defgroup haskell-debug nil
  "Settings for debugging support."
  :link '(custom-manual "(haskell-mode)haskell-debug")
  :group 'haskell)

;;;###autoload
(defface haskell-debug-warning-face
  '((t :inherit 'compilation-warning))
  "Face for warnings."
  :group 'haskell-debug)

;;;###autoload
(defface haskell-debug-trace-number-face
  '((t :weight bold :background "#f5f5f5"))
  "Face for numbers in backtrace."
  :group 'haskell-debug)

;;;###autoload
(defface haskell-debug-newline-face
  '((t :weight bold :background "#f0f0f0"))
  "Face for newlines in trace steps."
  :group 'haskell-debug)

;;;###autoload
(defface haskell-debug-keybinding-face
  '((t :inherit 'font-lock-type-face :weight bold))
  "Face for keybindings."
  :group 'haskell-debug)

;;;###autoload
(defface haskell-debug-heading-face
  '((t :inherit 'font-lock-keyword-face))
  "Face for headings."
  :group 'haskell-debug)

;;;###autoload
(defface haskell-debug-muted-face
  '((t :foreground "#999"))
  "Face for muteds."
  :group 'haskell-debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode

(defvar haskell-debug-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'haskell-debug/refresh)
    (define-key map (kbd "s") 'haskell-debug/step)
    (define-key map (kbd "t") 'haskell-debug/trace)
    (define-key map (kbd "d") 'haskell-debug/delete)
    (define-key map (kbd "b") 'haskell-debug/break-on-function)
    (define-key map (kbd "a") 'haskell-debug/abandon)
    (define-key map (kbd "c") 'haskell-debug/continue)
    (define-key map (kbd "p") 'haskell-debug/previous)
    (define-key map (kbd "n") 'haskell-debug/next)
    (define-key map (kbd "RET") 'haskell-debug/select)
    map)
  "Keymap for `haskell-debug-mode'.")

(define-derived-mode haskell-debug-mode
  text-mode "Debug"
  "Major mode for debugging Haskell via GHCi.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals

(defvar haskell-debug-history-cache nil
  "Cache of the tracing history.")

(defvar haskell-debug-bindings-cache nil
  "Cache of the current step's bindings.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros

(defmacro haskell-debug-with-breakpoints (&rest body)
  "Breakpoints need to exist to start stepping."
  `(if (haskell-debug-get-breakpoints)
       ,@body
     (error "No breakpoints to step into!")))

(defmacro haskell-debug-with-modules (&rest body)
  "Modules need to exist to do debugging stuff."
  `(if (haskell-debug-get-modules)
       ,@body
     (error "No modules loaded!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

(defun haskell-debug/select ()
  "Select whatever is at point."
  (interactive)
  (cond
   ((get-text-property (point) 'break)
    (let ((break (get-text-property (point) 'break)))
      (haskell-debug-highlight (plist-get break :path)
                               (plist-get break :span))))
   ((get-text-property (point) 'module)
    (let ((break (get-text-property (point) 'module)))
      (haskell-debug-highlight (plist-get break :path))))))

(defun haskell-debug/abandon ()
  "Abandon the current computation."
  (interactive)
  (haskell-debug-with-breakpoints
   (haskell-process-queue-sync-request (haskell-debug-process) ":abandon")
   (message "Computation abandoned.")
   (setq haskell-debug-history-cache nil)
   (setq haskell-debug-bindings-cache nil)
   (haskell-debug/refresh)))

(defun haskell-debug/continue ()
  "Continue the current computation."
  (interactive)
  (haskell-debug-with-breakpoints
   (haskell-process-queue-sync-request (haskell-debug-process) ":continue")
   (message "Computation continued.")
   (setq haskell-debug-history-cache nil)
   (setq haskell-debug-bindings-cache nil)
   (haskell-debug/refresh)))

(defun haskell-debug/break-on-function ()
  "Break on function IDENT."
  (interactive)
  (haskell-debug-with-modules
   (let ((ident (read-from-minibuffer "Function: "
                                      (haskell-ident-at-point))))
     (haskell-process-queue-sync-request
      (haskell-debug-process)
      (concat ":break "
              ident))
     (message "Breaking on function: %s" ident)
     (haskell-debug/refresh))))

(defun haskell-debug/start-step (expr)
  "Start stepping EXPR."
  (interactive (list (read-from-minibuffer "Expression to step through: ")))
  (haskell-debug/step expr))

(defun haskell-debug/breakpoint-numbers ()
  "List breakpoint numbers."
  (interactive)
  (let ((breakpoints (mapcar (lambda (breakpoint)
                               (number-to-string (plist-get breakpoint :number)))
                             (haskell-debug-get-breakpoints))))
    (if (null breakpoints)
        (message "No breakpoints.")
      (message "Breakpoint(s): %s"
               (mapconcat #'identity
                          breakpoints
                          ", ")))))

(defun haskell-debug/next ()
  "Go to next step to inspect bindings."
  (interactive)
  (haskell-debug-with-breakpoints
   (haskell-debug-navigate "forward")))

(defun haskell-debug/previous ()
  "Go to previous step to inspect the bindings."
  (interactive)
  (haskell-debug-with-breakpoints
   (haskell-debug-navigate "back")))

(defun haskell-debug/refresh ()
  "Refresh the debugger buffer."
  (interactive)
  (with-current-buffer (haskell-debug-buffer-name (haskell-debug-session))
    (cd (haskell-session-current-dir (haskell-debug-session)))
    (let ((inhibit-read-only t)
          (p (point)))
      (erase-buffer)
      (insert (propertize (concat "Debugging "
                                  (haskell-session-name (haskell-debug-session))
                                  "\n\n")
                          'face `((:weight bold))))
      (let ((modules (haskell-debug-get-modules))
            (breakpoints (haskell-debug-get-breakpoints))
            (context (haskell-debug-get-context))
            (history (haskell-debug-get-history)))
        (unless modules
          (insert (propertize "You have to load a module to start debugging."
                              'face
                              'haskell-debug-warning-face)
                  "\n\n"))
        (haskell-debug-insert-bindings modules breakpoints context)
        (when modules
          (haskell-debug-insert-current-context context history)
          (haskell-debug-insert-breakpoints breakpoints))
        (haskell-debug-insert-modules modules))
      (insert "\n")
      (goto-char (min (point-max) p)))))

(defun haskell-debug/delete ()
  "Delete whatever's at the point."
  (interactive)
  (cond
   ((get-text-property (point) 'break)
    (let ((break (get-text-property (point) 'break)))
      (haskell-mode-toggle-interactive-prompt-state)
      (unwind-protect
          (when (y-or-n-p (format "Delete breakpoint #%d?"
                                  (plist-get break :number)))
            (haskell-process-queue-sync-request
             (haskell-debug-process)
             (format ":delete %d"
                     (plist-get break :number)))
            (haskell-debug/refresh))
        (haskell-mode-toggle-interactive-prompt-state t))))))

(defun haskell-debug/trace ()
  "Trace the expression."
  (interactive)
  (haskell-debug-with-modules
   (haskell-debug-with-breakpoints
    (let ((expr (read-from-minibuffer "Expression to trace: "
                                      (haskell-ident-at-point))))
      (haskell-process-queue-sync-request
       (haskell-debug-process)
       (concat ":trace " expr))
      (message "Tracing expression: %s" expr)
      (haskell-debug/refresh)))))

(defun haskell-debug/step (&optional expr)
  "Step into the next function."
  (interactive)
  (haskell-debug-with-breakpoints
   (let* ((breakpoints (haskell-debug-get-breakpoints))
          (context (haskell-debug-get-context))
          (string
           (haskell-process-queue-sync-request
            (haskell-debug-process)
            (if expr
                (concat ":step " expr)
              ":step"))))
     (cond
      ((string= string "not stopped at a breakpoint\n")
       (if haskell-debug-bindings-cache
           (progn (setq haskell-debug-bindings-cache nil)
                  (haskell-debug/refresh))
         (call-interactively 'haskell-debug/start-step)))
      (t (let ((maybe-stopped-at (haskell-debug-parse-stopped-at string)))
           (cond
            (maybe-stopped-at
             (setq haskell-debug-bindings-cache
                   maybe-stopped-at)
             (message "Computation paused.")
             (haskell-debug/refresh))
            (t
             (if context
                 (message "Computation finished.")
               (progn
                 (haskell-mode-toggle-interactive-prompt-state)
                 (unwind-protect
                     (when (y-or-n-p "Computation completed without breaking. Reload the module and retry?")
                       (message "Reloading and resetting breakpoints...")
                       (haskell-interactive-mode-reset-error (haskell-debug-session))
                       (cl-loop for break in breakpoints
                                do (haskell-process-queue-sync-request
                                    (haskell-debug-process)
                                    (concat ":load " (plist-get break :path))))
                       (cl-loop for break in breakpoints
                                do (haskell-debug-break break))
                       (haskell-debug/step expr))
                   (haskell-mode-toggle-interactive-prompt-state t))))))))))
   (haskell-debug/refresh)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal functions

(defun haskell-debug-session ()
  "Get the Haskell session."
  (or (haskell-session-maybe)
      (error "No Haskell session associated with this debug
      buffer. Please just close the buffer and start again.")))

(defun haskell-debug-process ()
  "Get the Haskell session."
  (or (haskell-session-process (haskell-session-maybe))
      (error "No Haskell session associated with this debug
      buffer. Please just close the buffer and start again.")))

(defun haskell-debug-buffer-name (session)
  "The debug buffer name for the current session."
  (format "*debug:%s*"
          (haskell-session-name session)))

(defun haskell-debug-get-breakpoints ()
  "Get the list of breakpoints currently set."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-debug-process)
                 ":show breaks")))
    (if (string= string "No active breakpoints.\n")
        (list)
      (mapcar #'haskell-debug-parse-break-point
              (haskell-debug-split-string string)))))

(defun haskell-debug-get-modules ()
  "Get the list of modules currently set."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-debug-process)
                 ":show modules")))
    (if (string= string "")
        (list)
      (mapcar #'haskell-debug-parse-module
              (haskell-debug-split-string string)))))

(defun haskell-debug-get-context ()
  "Get the current context."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-debug-process)
                 ":show context")))
    (if (string= string "")
        nil
      (haskell-debug-parse-context string))))

(defun haskell-debug-get-history ()
  "Get the step history."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-debug-process)
                 ":history")))
    (if (or (string= string "")
            (string= string "Not stopped at a breakpoint\n"))
        nil
      (if (string= string "Empty history. Perhaps you forgot to use :trace?\n")
          nil
        (let ((entries (mapcar #'haskell-debug-parse-history-entry
                               (cl-remove-if (lambda (line) (or (string= "<end of history>" line)
                                                                (string= "..." line)))
                                             (haskell-debug-split-string string)))))
          (setq haskell-debug-history-cache
                entries)
          entries)))))

(defun haskell-debug-insert-bindings (modules breakpoints context)
  "Insert a list of bindings."
  (if breakpoints
      (progn (haskell-debug-insert-binding "t" "trace an expression")
             (haskell-debug-insert-binding "s" "step into an expression")
             (haskell-debug-insert-binding "b" "breakpoint" t))
    (progn
      (when modules
        (haskell-debug-insert-binding "b" "breakpoint"))
      (when breakpoints
        (haskell-debug-insert-binding "s" "step into an expression" t))))
  (when breakpoints
    (haskell-debug-insert-binding "d" "delete breakpoint"))
  (when context
    (haskell-debug-insert-binding "a" "abandon context")
    (haskell-debug-insert-binding "c" "continue" t))
  (when context
    (haskell-debug-insert-binding "p" "previous step")
    (haskell-debug-insert-binding "n" "next step" t))
  (haskell-debug-insert-binding "g" "refresh" t)
  (insert "\n"))

(defun haskell-debug-insert-current-context (context history)
  "Insert the current context."
  (haskell-debug-insert-header "Context")
  (if context
      (haskell-debug-insert-context context history)
    (haskell-debug-insert-debug-finished))
  (insert "\n"))

(defun haskell-debug-insert-breakpoints (breakpoints)
  "insert the list of breakpoints."
  (haskell-debug-insert-header "Breakpoints")
  (if (null breakpoints)
      (haskell-debug-insert-muted "No active breakpoints.")
    (cl-loop for break in breakpoints
             do (insert (propertize (format "%d"
                                            (plist-get break :number))
                                    'face `((:weight bold))
                                    'break break)
                        (haskell-debug-muted " - ")
                        (propertize (plist-get break :module)
                                    'break break
                                    'break break)
                        (haskell-debug-muted
                         (format " (%d:%d)"
                                 (plist-get (plist-get break :span) :start-line)
                                 (plist-get (plist-get break :span) :start-col)))
                        "\n")))
  (insert "\n"))

(defun haskell-debug-insert-modules (modules)
  "Insert the list of modules."
  (haskell-debug-insert-header "Modules")
  (if (null modules)
      (haskell-debug-insert-muted "No loaded modules.")
    (progn (cl-loop for module in modules
                    do (insert (propertize (plist-get module :module)
                                           'module module
                                           'face `((:weight bold)))
                               (haskell-debug-muted " - ")
                               (propertize (file-name-nondirectory (plist-get module :path))
                                           'module module))
                    do (insert "\n")))))

(defun haskell-debug-split-string (string)
  "Split GHCi's line-based output, stripping the trailing newline."
  (split-string string "\n" t))

(defun haskell-debug-parse-context (string)
  "Parse the context."
  (cond
   ((string-match "^--> \\(.+\\)\n  \\(.+\\)" string)
    (let ((name (match-string 1 string))
          (stopped (haskell-debug-parse-stopped-at (match-string 2 string))))
      (list :name name
            :path (plist-get stopped :path)
            :span (plist-get stopped :span))))))

(defun haskell-debug-insert-binding (binding desc &optional end)
  "Insert a helpful keybinding."
  (insert (propertize binding 'face 'haskell-debug-keybinding-face)
          (haskell-debug-muted " - ")
          desc
          (if end
              "\n"
            (haskell-debug-muted ", "))))

(defun haskell-debug-insert-header (title)
  "Insert a header title."
  (insert (propertize title
                      'face 'haskell-debug-heading-face)
          "\n\n"))

(defun haskell-debug-insert-context (context history)
  "Insert the context and history."
  (when context
    (insert (propertize (plist-get context :name) 'face `((:weight bold)))
            (haskell-debug-muted " - ")
            (file-name-nondirectory (plist-get context :path))
            (haskell-debug-muted " (stopped)")
            "\n"))
  (when haskell-debug-bindings-cache
    (insert "\n")
    (let ((bindings haskell-debug-bindings-cache))
      (insert
       (haskell-debug-get-span-string
        (plist-get bindings :path)
        (plist-get bindings :span)))
      (insert "\n\n")
      (cl-loop for binding in (plist-get bindings :types)
               do (insert (haskell-fontify-as-mode binding 'haskell-mode)
                          "\n"))))
  (let ((history (or history
                     (list (haskell-debug-make-fake-history context)))))
    (when history
      (insert "\n")
      (haskell-debug-insert-history history))))

(defun haskell-debug-insert-debug-finished ()
  "Insert message that no debugging is happening, but if there is
some old history, then display that."
  (if haskell-debug-history-cache
      (progn (haskell-debug-insert-muted "Finished debugging.")
             (insert "\n")
             (haskell-debug-insert-history haskell-debug-history-cache))
    (haskell-debug-insert-muted "Not debugging right now.")))

(defun haskell-debug-insert-muted (text)
  "Insert some muted text."
  (insert (haskell-debug-muted text)
          "\n"))

(defun haskell-debug-muted (text)
  "Make some muted text."
  (propertize text 'face 'haskell-debug-muted-face))

(defun haskell-debug-parse-logged (string)
  "Parse the logged breakpoint."
  (cond
   ((string= "no more logged breakpoints\n" string)
    nil)
   ((string= "already at the beginning of the history\n" string)
    nil)
   (t
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (list :path (progn (search-forward " at ")
                         (buffer-substring-no-properties
                          (point)
                          (1- (search-forward ":"))))
            :span (haskell-debug-parse-span
                   (buffer-substring-no-properties
                    (point)
                    (line-end-position)))
            :types (progn (forward-line)
                          (haskell-debug-split-string
                           (buffer-substring-no-properties
                            (point)
                            (point-max)))))))))

(defun haskell-debug-parse-stopped-at (string)
  "Parse the location stopped at from the given string.

For example:

Stopped at /home/foo/project/src/x.hs:6:25-36

"
  (let ((index (string-match "Stopped at \\([^:]+\\):\\(.+\\)\n?"
                             string)))
    (when index
      (list :path (match-string 1 string)
            :span (haskell-debug-parse-span (match-string 2 string))
            :types (cdr (haskell-debug-split-string (substring string index)))))))

(defun haskell-debug-get-span-string (path span)
  "Get the string from the PATH and the SPAN."
  (save-window-excursion
    (find-file path)
    (buffer-substring
     (save-excursion
       (goto-char (point-min))
       (forward-line (1- (plist-get span :start-line)))
       (forward-char (1- (plist-get span :start-col)))
       (point))
     (save-excursion
       (goto-char (point-min))
       (forward-line (1- (plist-get span :end-line)))
       (forward-char (plist-get span :end-col))
       (point)))))

(defun haskell-debug-make-fake-history (context)
  "Make a fake history item."
  (list :index -1
        :path (plist-get context :path)
        :span (plist-get context :span)))

(defun haskell-debug-insert-history (history)
  "Insert tracing HISTORY."
  (let ((i (length history)))
    (cl-loop for span in history
             do (let ((string (haskell-debug-get-span-string
                               (plist-get span :path)
                               (plist-get span :span))))
                  (insert (propertize (format "%4d" i)
                                      'face 'haskell-debug-trace-number-face)
                          " "
                          (haskell-debug-preview-span
                           (plist-get span :span)
                           string
                           t)
                          "\n")
                  (setq i (1- i))))))

(defun haskell-debug-parse-span (string)
  "Parse a source span from a string.

Examples:

  (5,1)-(6,37)
  6:25-36
  5:20

People like to make other people's lives interesting by making
variances in source span notation."
  (cond
   ((string-match "\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)"
                  string)
    (list :start-line (string-to-number (match-string 1 string))
          :start-col (string-to-number (match-string 2 string))
          :end-line (string-to-number (match-string 1 string))
          :end-col (string-to-number (match-string 3 string))))
   ((string-match "\\([0-9]+\\):\\([0-9]+\\)"
                  string)
    (list :start-line (string-to-number (match-string 1 string))
          :start-col (string-to-number (match-string 2 string))
          :end-line (string-to-number (match-string 1 string))
          :end-col (string-to-number (match-string 2 string))))
   ((string-match "(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                  string)
    (list :start-line (string-to-number (match-string 1 string))
          :start-col (string-to-number (match-string 2 string))
          :end-line (string-to-number (match-string 3 string))
          :end-col (string-to-number (match-string 4 string))))
   (t (error "Unable to parse source span from string: %s"
             string))))

(defun haskell-debug-preview-span (span string &optional collapsed)
  "Make a one-line preview of the given expression."
  (with-temp-buffer
    (haskell-mode)
    (insert string)
    (when (/= 0 (plist-get span :start-col))
      (indent-rigidly (point-min)
                      (point-max)
                      1))
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (when (/= 0 (plist-get span :start-col))
      (indent-rigidly (point-min)
                      (point-max)
                      -1))
    (goto-char (point-min))
    (if collapsed
        (replace-regexp-in-string
         "\n[ ]*"
         (propertize " " 'face 'haskell-debug-newline-face)
         (buffer-substring (point-min)
                           (point-max)))
      (buffer-string))))

(defun haskell-debug-start (session)
  "Start the debug mode."
  (setq buffer-read-only t)
  (haskell-session-assign session)
  (haskell-debug/refresh))

(defun haskell-debug ()
  "Start the debugger for the current Haskell (GHCi) session."
  (interactive)
  (let ((session (haskell-debug-session)))
    (switch-to-buffer-other-window (haskell-debug-buffer-name session))
    (unless (eq major-mode 'haskell-debug-mode)
      (haskell-debug-mode)
      (haskell-debug-start session))))

(defun haskell-debug-break (break)
  "Set BREAK breakpoint in module at line/col."
  (haskell-process-queue-without-filters
   (haskell-debug-process)
   (format ":break %s %s %d"
           (plist-get break :module)
           (plist-get (plist-get break :span) :start-line)
           (plist-get (plist-get break :span) :start-col))))

(defun haskell-debug-navigate (direction)
  "Navigate in DIRECTION \"back\" or \"forward\"."
  (let ((string (haskell-process-queue-sync-request
                 (haskell-debug-process)
                 (concat ":" direction))))
    (let ((bindings (haskell-debug-parse-logged string)))
      (setq haskell-debug-bindings-cache
            bindings)
      (when (not bindings)
        (message "No more %s results!" direction)))
    (haskell-debug/refresh)))

(defun haskell-debug-session-debugging-p (session)
  "Does the session have a debugging buffer open?"
  (not (not (get-buffer (haskell-debug-buffer-name session)))))

(defun haskell-debug-highlight (path &optional span)
  "Highlight the file at span."
  (let ((p (make-overlay
            (line-beginning-position)
            (line-end-position))))
    (overlay-put p 'face `((:background "#eee")))
    (with-current-buffer
        (if span
            (save-window-excursion
              (find-file path)
              (current-buffer))
          (find-file path)
          (current-buffer))
      (let ((o (when span
                 (make-overlay
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- (plist-get span :start-line)))
                    (forward-char (1- (plist-get span :start-col)))
                    (point))
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- (plist-get span :end-line)))
                    (forward-char (plist-get span :end-col))
                    (point))))))
        (when o
          (overlay-put o 'face `((:background "#eee"))))
        (sit-for 0.5)
        (when o
          (delete-overlay o))
        (delete-overlay p)))))

(defun haskell-debug-parse-history-entry (string)
  "Parse a history entry."
  (if (string-match "^\\([-0-9]+\\)[ ]+:[ ]+\\([A-Za-z0-9_':]+\\)[ ]+(\\([^:]+\\):\\(.+?\\))$"
                    string)
      (list :index (string-to-number (match-string 1 string))
            :name (match-string 2 string)
            :path (match-string 3 string)
            :span (haskell-debug-parse-span (match-string 4 string)))
    (error "Unable to parse history entry: %s" string)))

(defun haskell-debug-parse-module (string)
  "Parse a module and path.

For example:

X                ( /home/foo/X.hs, interpreted )
Main             ( /home/foo/X.hs, /home/foo/X.o )
"
  (if (string-match "\\([^ ]+\\)[ ]+( \\([^ ]+?\\), [/a-zA-Z0-9\.]+ )$"
                    string)
      (list :module (match-string 1 string)
            :path (match-string 2 string))
    (error "Unable to parse module from string: %s"
           string)))

(defun haskell-debug-parse-break-point (string)
  "Parse a breakpoint number, module and location from a string.

For example:

[13] Main /home/foo/src/x.hs:(5,1)-(6,37)

"
  (if (string-match "^\\[\\([0-9]+\\)\\] \\([^ ]+\\) \\([^:]+\\):\\(.+\\)$"
                    string)
      (list :number (string-to-number (match-string 1 string))
            :module (match-string 2 string)
            :path (match-string 3 string)
            :span (haskell-debug-parse-span (match-string 4 string)))
    (error "Unable to parse breakpoint from string: %s"
           string)))

(provide 'haskell-debug)

;;; haskell-debug.el ends here
