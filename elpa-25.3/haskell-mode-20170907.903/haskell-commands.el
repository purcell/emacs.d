;;; haskell-commands.el --- Commands that can be run on the process -*- lexical-binding: t -*-

;;; Commentary:

;;; This module provides varoius `haskell-mode' and `haskell-interactive-mode'
;;; specific commands such as show type signature, show info, haskell process
;;; commands and etc.

;; Copyright © 2014 Chris Done.  All rights reserved.
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
(require 'etags)
(require 'haskell-mode)
(require 'haskell-compat)
(require 'haskell-process)
(require 'haskell-font-lock)
(require 'haskell-interactive-mode)
(require 'haskell-session)
(require 'haskell-string)
(require 'haskell-presentation-mode)
(require 'haskell-utils)
(require 'highlight-uses-mode)
(require 'haskell-cabal)

(defcustom haskell-mode-stylish-haskell-path "stylish-haskell"
  "Path to `stylish-haskell' executable."
  :group 'haskell
  :type 'string)

(defcustom haskell-interactive-set-+c
  t
  "Issue ':set +c' in interactive session to support type introspection."
  :group 'haskell-interactive
  :type 'boolean)

;;;###autoload
(defun haskell-process-restart ()
  "Restart the inferior Haskell process."
  (interactive)
  (haskell-process-reset (haskell-interactive-process))
  (haskell-process-set (haskell-interactive-process) 'command-queue nil)
  (haskell-process-start (haskell-interactive-session)))

(defun haskell-process-start (session)
  "Start the inferior Haskell process with a given SESSION.
You can create new session using function `haskell-session-make'."
  (let ((existing-process (get-process (haskell-session-name (haskell-interactive-session)))))
    (when (processp existing-process)
      (haskell-interactive-mode-echo session "Restarting process ...")
      (haskell-process-set (haskell-session-process session) 'is-restarting t)
      (delete-process existing-process)))
  (let ((process (or (haskell-session-process session)
                     (haskell-process-make (haskell-session-name session))))
        (old-queue (haskell-process-get (haskell-session-process session)
                                        'command-queue)))
    (haskell-session-set-process session process)
    (haskell-process-set-session process session)
    (haskell-process-set-cmd process nil)
    (haskell-process-set (haskell-session-process session) 'is-restarting nil)
    (let ((default-directory (haskell-session-cabal-dir session))
          (log-and-command (haskell-process-compute-process-log-and-command session (haskell-process-type))))
      (haskell-session-prompt-set-current-dir session (not haskell-process-load-or-reload-prompt))
      (haskell-process-set-process
       process
       (progn
         (haskell-process-log (propertize (format "%S" log-and-command)))
         (apply #'start-process (cdr log-and-command)))))
    (progn (set-process-sentinel (haskell-process-process process) 'haskell-process-sentinel)
           (set-process-filter (haskell-process-process process) 'haskell-process-filter))
    (haskell-process-send-startup process)
    (unless (or (eq 'cabal-repl (haskell-process-type))
                (eq 'cabal-new-repl (haskell-process-type))
                   (eq 'stack-ghci (haskell-process-type))) ;; Both "cabal repl" and "stack ghci" set the proper CWD.
      (haskell-process-change-dir session
                                  process
                                  (haskell-session-current-dir session)))
    (haskell-process-set process 'command-queue
                         (append (haskell-process-get (haskell-session-process session)
                                                      'command-queue)
                                 old-queue))
    process))

(defun haskell-process-send-startup (process)
  "Send the necessary start messages to haskell PROCESS."
  (haskell-process-queue-command
   process
   (make-haskell-command
    :state process

    :go (lambda (process)
          ;; We must set the prompt last, so that this command as a
          ;; whole produces only one prompt marker as a response.
          (haskell-process-send-string process
                                       (mapconcat #'identity
                                                  (append '("Prelude.putStrLn \"\""
                                                            ":set -v1")
                                                          (when haskell-interactive-set-+c
                                                            '(":set +c"))) ; :type-at in GHC 8+
                                                  "\n"))
          (haskell-process-send-string process ":set prompt \"\\4\"")
          (haskell-process-send-string process (format ":set prompt2 \"%s\""
                                                       haskell-interactive-prompt2)))

    :live (lambda (process buffer)
            (when (haskell-process-consume
                   process
                   "^\*\*\* WARNING: \\(.+\\) is writable by someone else, IGNORING!$")
              (let ((path (match-string 1 buffer)))
                (haskell-session-modify
                 (haskell-process-session process)
                 'ignored-files
                 (lambda (files)
                   (cl-remove-duplicates (cons path files) :test 'string=)))
                (haskell-interactive-mode-compile-warning
                 (haskell-process-session process)
                 (format "GHCi is ignoring: %s (run M-x haskell-process-unignore)"
                         path)))))

    :complete (lambda (process _)
                (haskell-interactive-mode-echo
                 (haskell-process-session process)
                 (concat (nth (random (length haskell-process-greetings))
                              haskell-process-greetings)
                         (when haskell-process-show-debug-tips
                           "
If I break, you can:
  1. Restart:           M-x haskell-process-restart
  2. Configure logging: C-h v haskell-process-log (useful for debugging)
  3. General config:    M-x customize-mode
  4. Hide these tips:   C-h v haskell-process-show-debug-tips")))
                (with-current-buffer (haskell-interactive-buffer)
                  (goto-char haskell-interactive-mode-prompt-start))))))

(defun haskell-commands-process ()
  "Get the Haskell session, throws an error if not available."
  (or (haskell-session-process (haskell-session-maybe))
      (error "No Haskell session/process associated with this
      buffer. Maybe run M-x haskell-session-change?")))

;;;###autoload
(defun haskell-process-clear ()
  "Clear the current process."
  (interactive)
  (haskell-process-reset (haskell-commands-process))
  (haskell-process-set (haskell-commands-process) 'command-queue nil))

;;;###autoload
(defun haskell-process-interrupt ()
  "Interrupt the process (SIGINT)."
  (interactive)
  (interrupt-process (haskell-process-process (haskell-commands-process))))

(defun haskell-process-reload-with-fbytecode (process module-buffer)
  "Query a PROCESS to reload MODULE-BUFFER with -fbyte-code set.
Restores -fobject-code after reload finished.
MODULE-BUFFER is the actual Emacs buffer of the module being loaded."
  (haskell-process-queue-without-filters process ":set -fbyte-code")
  ;; We prefix the module's filename with a "*", which asks ghci to
  ;; ignore any existing object file and interpret the module.
  ;; Dependencies will still use their object files as usual.
  (haskell-process-queue-without-filters
   process
   (format ":load \"*%s\""
           (replace-regexp-in-string
            "\""
            "\\\\\""
            (buffer-file-name module-buffer))))
  (haskell-process-queue-without-filters process ":set -fobject-code"))

(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defvar haskell-cabal-targets-history nil
  "History list for session targets.")

(defun haskell-process-hayoo-ident (ident)
  "Hayoo for IDENT, return a list of modules"
  ;; We need a real/simulated closure, because otherwise these
  ;; variables will be unbound when the url-retrieve callback is
  ;; called.
  ;; TODO: Remove when this code is converted to lexical bindings by
  ;; default (Emacs 24.1+)
  (let ((url (format haskell-process-hayoo-query-url (url-hexify-string ident))))
    (with-current-buffer (url-retrieve-synchronously url)
      (if (= 200 url-http-response-status)
          (progn
            (goto-char url-http-end-of-headers)
            (let* ((res (json-read))
                   (results (assoc-default 'result res)))
              ;; TODO: gather packages as well, and when we choose a
              ;; given import, check that we have the package in the
              ;; cabal file as well.
              (cl-mapcan (lambda (r)
                           ;; append converts from vector -> list
                           (append (assoc-default 'resultModules r) nil))
                         results)))
        (warn "HTTP error %s fetching %s" url-http-response-status url)))))

(defun haskell-process-hoogle-ident (ident)
  "Hoogle for IDENT, return a list of modules."
  (with-temp-buffer
    (let ((hoogle-error (call-process "hoogle" nil t nil "search" "--exact" ident)))
      (goto-char (point-min))
      (unless (or (/= 0 hoogle-error)
                  (looking-at "^No results found")
                  (looking-at "^package "))
        (while (re-search-forward "^\\([^ ]+\\).*$" nil t)
          (replace-match "\\1" nil nil))
        (cl-remove-if (lambda (a) (string= "" a))
                      (split-string (buffer-string)
                                    "\n"))))))

(defun haskell-process-haskell-docs-ident (ident)
  "Search with haskell-docs for IDENT, return a list of modules."
  (cl-remove-if-not
   (lambda (a) (string-match "^[[:upper:]][[:alnum:]_'.]+$" a))
   (split-string
      (with-output-to-string
        (with-current-buffer
            standard-output
          (call-process "haskell-docs"
                        nil             ; no infile
                        t               ; output to current buffer (that is string)
                        nil             ; do not redisplay
                        "--modules" ident)))
      "\n")))

(defun haskell-process-import-modules (process modules)
  "Query PROCESS `:m +' command to import MODULES."
  (when haskell-process-auto-import-loaded-modules
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process modules)
      :go (lambda (state)
            (haskell-process-send-string
             (car state)
             (format ":m + %s" (mapconcat 'identity (cdr state) " "))))))))

;;;###autoload
(defun haskell-describe (ident)
  "Describe the given identifier IDENT."
  (interactive (list (read-from-minibuffer "Describe identifier: "
                                           (haskell-ident-at-point))))
  (let ((results (read (shell-command-to-string
                        (concat "haskell-docs --sexp "
                                ident)))))
    (help-setup-xref (list #'haskell-describe ident)
                     (called-interactively-p 'interactive))
    (save-excursion
      (with-help-window (help-buffer)
        (with-current-buffer (help-buffer)
          (if results
              (cl-loop for result in results
                       do (insert (propertize ident 'font-lock-face
                                              '((:inherit font-lock-type-face
                                                          :underline t)))
                                  " is defined in "
                                  (let ((module (cadr (assoc 'module result))))
                                    (if module
                                        (concat module " ")
                                      ""))
                                  (cadr (assoc 'package result))
                                  "\n\n")
                       do (let ((type (cadr (assoc 'type result))))
                            (when type
                              (insert (haskell-fontify-as-mode type 'haskell-mode)
                                      "\n")))
                       do (let ((args (cadr (assoc 'type results))))
                            (cl-loop for arg in args
                                     do (insert arg "\n"))
                            (insert "\n"))
                       do (insert (cadr (assoc 'documentation result)))
                       do (insert "\n\n"))
            (insert "No results for " ident)))))))

;;;###autoload
(defun haskell-rgrep (&optional prompt)
  "Grep the effective project for the symbol at point.
Very useful for codebase navigation.

Prompts for an arbitrary regexp given a prefix arg PROMPT."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (rgrep sym
           "*.hs *.lhs *.hsc *.chs *.hs-boot *.lhs-boot"
           (haskell-session-current-dir (haskell-interactive-session)))))

;;;###autoload
(defun haskell-process-do-info (&optional prompt-value)
  "Print info on the identifier at point.
If PROMPT-VALUE is non-nil, request identifier via mini-buffer."
  (interactive "P")
  (let ((at-point (haskell-ident-at-point)))
    (when (or prompt-value at-point)
      (let* ((ident (replace-regexp-in-string
                     "^!\\([A-Z_a-z]\\)"
                     "\\1"
                     (if prompt-value
                         (read-from-minibuffer "Info: " at-point)
                       at-point)))
             (modname (unless prompt-value
                        (haskell-utils-parse-import-statement-at-point)))
             (command (cond
                       (modname
                        (format ":browse! %s" modname))
                       ((string= ident "") ; For the minibuffer input case
                        nil)
                       (t (format (if (string-match "^[a-zA-Z_]" ident)
                                      ":info %s"
                                    ":info (%s)")
                                  (or ident
                                      at-point))))))
        (when command
          (haskell-process-show-repl-response command))))))

;;;###autoload
(defun haskell-process-do-type (&optional insert-value)
  "Print the type of the given expression.

Given INSERT-VALUE prefix indicates that result type signature
should be inserted."
  (interactive "P")
  (if insert-value
      (haskell-process-insert-type)
    (let* ((expr
            (if (use-region-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (haskell-ident-at-point)))
           (expr-okay (and expr
                         (not (string-match-p "\\`[[:space:]]*\\'" expr))
                         (not (string-match-p "\n" expr)))))
      ;; No newlines in expressions, and surround with parens if it
      ;; might be a slice expression
      (when expr-okay
        (haskell-process-show-repl-response
         (format
          (if (or (string-match-p "\\`(" expr)
                  (string-match-p "\\`[_[:alpha:]]" expr))
              ":type %s"
            ":type (%s)")
          expr))))))

;;;###autoload
(defun haskell-mode-jump-to-def-or-tag (&optional _next-p)
  ;; FIXME NEXT-P arg is not used
  "Jump to the definition.
Jump to definition of identifier at point by consulting GHCi, or
tag table as fallback.

Remember: If GHCi is busy doing something, this will delay, but
it will always be accurate, in contrast to tags, which always
work but are not always accurate.
If the definition or tag is found, the location from which you jumped
will be pushed onto `xref--marker-ring', so you can return to that
position with `xref-pop-marker-stack'."
  (interactive "P")
  (if (haskell-session-maybe)
        (let ((initial-loc (point-marker))
            (loc (haskell-mode-find-def (haskell-ident-at-point))))
          (haskell-mode-handle-generic-loc loc)
          (unless (equal initial-loc (point-marker))
            (xref-push-marker-stack initial-loc)))
      (call-interactively 'haskell-mode-tag-find)))

;;;###autoload
(defun haskell-mode-goto-loc ()
  "Go to the location of the thing at point.
Requires the :loc-at command from GHCi."
  (interactive)
  (let ((loc (haskell-mode-loc-at)))
    (when loc
      (haskell-mode-goto-span loc))))

(defun haskell-mode-goto-span (span)
  "Jump to the SPAN, whatever file and line and column it needs to get there."
  (xref-push-marker-stack)
  (find-file (expand-file-name (plist-get span :path)
                               (haskell-session-cabal-dir (haskell-interactive-session))))
  (goto-char (point-min))
  (forward-line (1- (plist-get span :start-line)))
  (forward-char (plist-get span :start-col)))

(defun haskell-process-insert-type ()
  "Get the identifier at the point and insert its type.
Use GHCi's :type if it's possible."
  (let ((ident (haskell-ident-at-point)))
    (when ident
      (let ((process (haskell-interactive-process))
            (query (format (if (string-match "^[_[:lower:][:upper:]]" ident)
                               ":type %s"
                             ":type (%s)")
                           ident)))
        (haskell-process-queue-command
         process
         (make-haskell-command
          :state (list process query (current-buffer))
          :go (lambda (state)
                (haskell-process-send-string (nth 0 state)
                                             (nth 1 state)))
          :complete (lambda (state response)
                      (cond
                       ;; TODO: Generalize this into a function.
                       ((or (string-match "^Top level" response)
                            (string-match "^<interactive>" response))
                        (message "%s" response))
                       (t
                        (with-current-buffer (nth 2 state)
                          (goto-char (line-beginning-position))
                          (insert (format "%s\n" (replace-regexp-in-string "\n$" "" response)))))))))))))

(defun haskell-mode-find-def (ident)
  ;; TODO Check if it possible to exploit `haskell-process-do-info'
  "Find definition location of identifier IDENT.
Uses the GHCi process to find the location.  Returns nil if it
can't find the identifier or the identifier isn't a string.

Returns:

    (library <package> <module>)
    (file <path> <line> <col>)
    (module <name>)
    nil"
  (when (stringp ident)
    (let ((reply (haskell-process-queue-sync-request
                  (haskell-interactive-process)
                  (format (if (string-match "^[a-zA-Z_]" ident)
                              ":info %s"
                            ":info (%s)")
                          ident))))
      (let ((match (string-match "-- Defined \\(at\\|in\\) \\(.+\\)$" reply)))
        (when match
          (let ((defined (match-string 2 reply)))
            (let ((match (string-match "\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)$" defined)))
              (cond
               (match
                (list 'file
                      (expand-file-name (match-string 1 defined)
                                        (haskell-session-current-dir (haskell-interactive-session)))
                      (string-to-number (match-string 2 defined))
                      (string-to-number (match-string 3 defined))))
               (t
                (let ((match (string-match "`\\(.+?\\):\\(.+?\\)'$" defined)))
                  (if match
                      (list 'library
                            (match-string 1 defined)
                            (match-string 2 defined))
                    (let ((match (string-match "`\\(.+?\\)'$" defined)))
                      (if match
                          (list 'module
                                (match-string 1 defined)))))))))))))))

;;;###autoload
(defun haskell-mode-jump-to-def (ident)
  "Jump to definition of identifier IDENT at point."
  (interactive
   (list
    (haskell-string-drop-qualifier
     (haskell-ident-at-point))))
  (let ((loc (haskell-mode-find-def ident)))
    (when loc
      (haskell-mode-handle-generic-loc loc))))

(defun haskell-mode-handle-generic-loc (loc)
  "Either jump to or echo a generic location LOC.
Either a file or a library."
  (cl-case (car loc)
    (file (progn
              (find-file (elt loc 1))
              (goto-char (point-min))
              (forward-line (1- (elt loc 2)))
              (goto-char (+ (line-beginning-position)
                            (1- (elt loc 3))))))
    (library (message "Defined in `%s' (%s)."
                      (elt loc 2)
                      (elt loc 1)))
    (module (message "Defined in `%s'."
                     (elt loc 1)))))

(defun haskell-mode-loc-at ()
  "Get the location at point.
Requires the :loc-at command from GHCi."
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (haskell-spanable-pos-at-point)
                 (cons (point)
                       (point)))))
    (when pos
      (let ((reply (haskell-process-queue-sync-request
                    (haskell-interactive-process)
                    (save-excursion
                      (format ":loc-at %s %d %d %d %d %s"
                              (buffer-file-name)
                              (progn (goto-char (car pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (progn (goto-char (cdr pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (buffer-substring-no-properties (car pos)
                                                              (cdr pos)))))))
        (if reply
            (if (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                              reply)
                (list :path (match-string 1 reply)
                      :start-line (string-to-number (match-string 2 reply))
                      ;; ;; GHC uses 1-based columns.
                      :start-col (1- (string-to-number (match-string 3 reply)))
                      :end-line (string-to-number (match-string 4 reply))
                      ;; GHC uses 1-based columns.
                      :end-col (1- (string-to-number (match-string 5 reply))))
              (error (propertize reply 'face 'compilation-error)))
          (error (propertize "No reply. Is :loc-at supported?"
                             'face 'compilation-error)))))))

;;;###autoload
(defun haskell-process-cd (&optional _not-interactive)
  ;; FIXME optional arg is not used
  "Change directory."
  (interactive)
  (let* ((session (haskell-interactive-session))
         (dir (haskell-session-prompt-set-current-dir session)))
    (haskell-process-log
     (propertize (format "Changing directory to %s ...\n" dir)
                 'face font-lock-comment-face))
    (haskell-process-change-dir session
                                (haskell-interactive-process)
                                dir)))

(defun haskell-session-buffer-default-dir (session &optional buffer)
  "Try to deduce a sensible default directory for SESSION and BUFFER,
of which the latter defaults to the current buffer."
  (or (haskell-session-get session 'current-dir)
      (haskell-session-get session 'cabal-dir)
      (if (buffer-file-name buffer)
          (file-name-directory (buffer-file-name buffer))
          "~/")))

(defun haskell-session-prompt-set-current-dir (session &optional use-default)
  "Prompt for the current directory.
Return current working directory for SESSION."
  (let ((default (haskell-session-buffer-default-dir session)))
    (haskell-session-set-current-dir
     session
     (if use-default
         default
         (haskell-utils-read-directory-name "Set current directory: " default))))
  (haskell-session-get session 'current-dir))

(defun haskell-process-change-dir (session process dir)
  "Change SESSION's current directory.
Query PROCESS to `:cd` to directory DIR."
  (haskell-process-queue-command
   process
   (make-haskell-command
    :state (list session process dir)
    :go
    (lambda (state)
      (haskell-process-send-string
       (cadr state) (format ":cd %s" (cl-caddr state))))

    :complete
    (lambda (state _)
      (haskell-session-set-current-dir (car state) (cl-caddr state))
      (haskell-interactive-mode-echo (car state)
                                     (format "Changed directory: %s"
                                             (cl-caddr state)))))))

;;;###autoload
(defun haskell-process-cabal-macros ()
  "Send the cabal macros string."
  (interactive)
  (haskell-process-queue-without-filters (haskell-interactive-process)
                                         ":set -optP-include -optPdist/build/autogen/cabal_macros.h"))

(defun haskell-process-do-try-info (sym)
  "Get info of SYM and echo in the minibuffer."
  (let ((process (haskell-interactive-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process sym)
      :go (lambda (state)
            (haskell-process-send-string
             (car state)
             (if (string-match "^[A-Za-z_]" (cdr state))
                 (format ":info %s" (cdr state))
               (format ":info (%s)" (cdr state)))))
      :complete (lambda (_state response)
                  (unless (or (string-match "^Top level" response)
                              (string-match "^<interactive>" response))
                    (haskell-mode-message-line response)))))))

(defun haskell-process-do-try-type (sym)
  "Get type of SYM and echo in the minibuffer."
  (let ((process (haskell-interactive-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process sym)
      :go (lambda (state)
            (haskell-process-send-string
             (car state)
             (if (string-match "^[A-Za-z_]" (cdr state))
                 (format ":type %s" (cdr state))
               (format ":type (%s)" (cdr state)))))
      :complete (lambda (_state response)
                  (unless (or (string-match "^Top level" response)
                              (string-match "^<interactive>" response))
                    (haskell-mode-message-line response)))))))

;;;###autoload
(defun haskell-mode-show-type-at (&optional insert-value)
  "Show type of the thing at point or within active region asynchronously.
This function requires GHCi 8+ or GHCi-ng.

\\<haskell-interactive-mode-map>
To make this function works sometimes you need to load the file in REPL
first using command `haskell-process-load-file' bound to
\\[haskell-process-load-file].

Optional argument INSERT-VALUE indicates that
recieved type signature should be inserted (but only if nothing
happened since function invocation)."
  (interactive "P")
  (let* ((pos (haskell-command-capture-expr-bounds))
         (req (haskell-utils-compose-type-at-command pos))
         (process (haskell-interactive-process))
         (buf (current-buffer))
         (pos-reg (cons pos (region-active-p))))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (list process req buf insert-value pos-reg)
      :go
      (lambda (state)
        (let* ((prc (car state))
               (req (nth 1 state)))
          (haskell-utils-async-watch-changes)
          (haskell-process-send-string prc req)))
      :complete
      (lambda (state response)
        (let* ((init-buffer (nth 2 state))
               (insert-value (nth 3 state))
               (pos-reg (nth 4 state))
               (wrap (cdr pos-reg))
               (min-pos (caar pos-reg))
               (max-pos (cdar pos-reg))
               (sig (haskell-utils-reduce-string response))
               (res-type (haskell-utils-repl-response-error-status sig)))

          (cl-case res-type
            ;; neither popup presentation buffer
            ;; nor insert response in error case
            ('unknown-command
             (message "This command requires GHCi 8+ or GHCi-ng. Please read command description for details."))
            ('option-missing
             (message "Could not infer type signature. You need to load file first. Also :set +c is required, see customization `haskell-interactive-set-+c'. Please read command description for details."))
            ('interactive-error (message "Wrong REPL response: %s" sig))
            (otherwise
             (if insert-value
                 ;; Only insert type signature and do not present it
                 (if (= (length haskell-utils-async-post-command-flag) 1)
                     (if wrap
                         ;; Handle region case
                         (progn
                           (deactivate-mark)
                           (save-excursion
                             (delete-region min-pos max-pos)
                             (goto-char min-pos)
                             (insert (concat "(" sig ")"))))
                       ;; Non-region cases
                       (haskell-command-insert-type-signature sig))
                   ;; Some commands registered, prevent insertion
                   (message "Type signature insertion was prevented. These commands were registered: %s"
                            (cdr (reverse haskell-utils-async-post-command-flag))))
               ;; Present the result only when response is valid and not asked
               ;; to insert result
               (haskell-command-echo-or-present response)))

            (haskell-utils-async-stop-watching-changes init-buffer))))))))

(make-obsolete 'haskell-process-generate-tags
               'haskell-mode-generate-tags
               "2016-03-14")
(defun haskell-process-generate-tags (&optional and-then-find-this-tag)
  "Regenerate the TAGS table.
If optional AND-THEN-FIND-THIS-TAG argument is present it is used with
function `xref-find-definitions' after new table was generated."
  (interactive)
  (let ((process (haskell-interactive-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process and-then-find-this-tag)
      :go
      (lambda (state)
        (let* ((process (car state))
               (cabal-dir (haskell-session-cabal-dir
                           (haskell-process-session process)))
               (command (haskell-cabal--compose-hasktags-command cabal-dir)))
          (haskell-process-send-string process command)))
      :complete (lambda (state _response)
                  (when (cdr state)
                    (let ((tags-file-name
                           (haskell-session-tags-filename
                            (haskell-process-session (car state)))))
                      (xref-find-definitions (cdr state))))
                  (haskell-mode-message-line "Tags generated."))))))

(defun haskell-process-add-cabal-autogen ()
  "Add cabal's autogen dir to the GHCi search path.
Add <cabal-project-dir>/dist/build/autogen/ to GHCi seatch path.
This allows modules such as 'Path_...', generated by cabal, to be
loaded by GHCi."
  (unless (or (eq 'cabal-repl (haskell-process-type))
              (eq 'cabal-new-repl (haskell-process-type))) ;; redundant with "cabal repl"
    (let*
        ((session       (haskell-interactive-session))
         (cabal-dir     (haskell-session-cabal-dir session))
         (ghci-gen-dir  (format "%sdist/build/autogen/" cabal-dir)))
      (haskell-process-queue-without-filters
       (haskell-interactive-process)
       (format ":set -i%s" ghci-gen-dir)))))

;;;###autoload
(defun haskell-process-unignore ()
  "Unignore any ignored files.
Do not ignore files that were specified as being ignored by the
inferior GHCi process."
  (interactive)
  (let ((session (haskell-interactive-session))
        (changed nil))
    (if (null (haskell-session-get session 'ignored-files))
        (message "Nothing to unignore!")
      (cl-loop for file in (haskell-session-get session 'ignored-files)
               do
               (haskell-mode-toggle-interactive-prompt-state)
               (unwind-protect
                   (progn
                     (cl-case
                         (read-event
                          (propertize
                           (format "Set permissions? %s (y, n, v: stop and view file)"
                                   file)
                           'face
                           'minibuffer-prompt))
                       (?y
                        (haskell-process-unignore-file session file)
                        (setq changed t))
                       (?v
                        (find-file file)
                        (cl-return)))
                     (when (and changed
                                (y-or-n-p "Restart GHCi process now? "))
                       (haskell-process-restart)))
                 ;; unwind
                 (haskell-mode-toggle-interactive-prompt-state t))))))

;;;###autoload
(defun haskell-session-change-target (target)
  "Set the build TARGET for cabal REPL."
  (interactive
   (list
    (completing-read "New build target: "
                     (haskell-cabal-enum-targets (haskell-process-type))
                     nil
                     nil
                     nil
                     'haskell-cabal-targets-history)))
  (let* ((session haskell-session)
         (old-target (haskell-session-get session 'target)))
    (when session
      (haskell-session-set-target session target)
      (when (not (string= old-target target))
        (haskell-mode-toggle-interactive-prompt-state)
        (unwind-protect
            (when (y-or-n-p "Target changed, restart haskell process?")
              (haskell-process-start session)))
        (haskell-mode-toggle-interactive-prompt-state t)))))

;;;###autoload
(defun haskell-mode-stylish-buffer ()
  "Apply stylish-haskell to the current buffer.

Use `haskell-mode-stylish-haskell-path' to know where to find
stylish-haskell executable. This function tries to preserve
cursor position and markers by using
`haskell-mode-buffer-apply-command'."
  (interactive)
  (haskell-mode-buffer-apply-command haskell-mode-stylish-haskell-path))

(defun haskell-mode-buffer-apply-command (cmd)
  "Execute shell command CMD with current buffer as input and output.
Use buffer as input and replace the whole buffer with the
output.  If CMD fails the buffer remains unchanged."
  (set-buffer-modified-p t)
  (let* ((out-file (make-temp-file "stylish-output"))
         (err-file (make-temp-file "stylish-error")))
        (unwind-protect
          (let* ((_errcode
                  (call-process-region (point-min) (point-max) cmd nil
                                       `((:file ,out-file) ,err-file)
                                       nil))
                 (err-file-empty-p
                  (equal 0 (nth 7 (file-attributes err-file))))
                 (out-file-empty-p
                  (equal 0 (nth 7 (file-attributes out-file)))))
            (if err-file-empty-p
                (if out-file-empty-p
                    (message "Error: %s produced no output and no error information, leaving buffer alone" cmd)
                  ;; Command successful, insert file with replacement to preserve
                  ;; markers.
                  (insert-file-contents out-file nil nil nil t))
              (progn
                ;; non-null stderr, command must have failed
                (message "Error: %s ended with errors, leaving buffer alone" cmd)
                (with-temp-buffer
                  (insert-file-contents err-file)
                  ;; use (warning-minimum-level :debug) to see this
                  (display-warning cmd
                                   (buffer-substring-no-properties (point-min) (point-max))
                                   :debug)))))
          (ignore-errors
            (delete-file err-file))
          (ignore-errors
            (delete-file out-file)))))

;;;###autoload
(defun haskell-mode-find-uses ()
  "Find use cases of the identifier at point and highlight them all."
  (interactive)
  (let ((spans (haskell-mode-uses-at)))
    (unless (null spans)
      (highlight-uses-mode 1)
      (cl-loop for span in spans
               do (haskell-mode-make-use-highlight span)))))

(defun haskell-mode-make-use-highlight (span)
  "Make a highlight overlay at the given SPAN."
  (save-window-excursion
    (save-excursion
      (haskell-mode-goto-span span)
      (save-excursion
        (highlight-uses-mode-highlight
         (progn
           (goto-char (point-min))
           (forward-line (1- (plist-get span :start-line)))
           (forward-char (plist-get span :start-col))
           (point))
         (progn
           (goto-char (point-min))
           (forward-line (1- (plist-get span :end-line)))
           (forward-char (plist-get span :end-col))
           (point)))))))

(defun haskell-mode-uses-at ()
  "Get the locations of use cases for the ident at point.
Requires the :uses command from GHCi."
  (let ((pos (or (when (region-active-p)
                   (cons (region-beginning)
                         (region-end)))
                 (haskell-ident-pos-at-point)
                 (cons (point)
                       (point)))))
    (when pos
      (let ((reply (haskell-process-queue-sync-request
                    (haskell-interactive-process)
                    (save-excursion
                      (format ":uses %s %d %d %d %d %s"
                              (buffer-file-name)
                              (progn (goto-char (car pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (progn (goto-char (cdr pos))
                                     (line-number-at-pos))
                              (1+ (current-column)) ;; GHC uses 1-based columns.
                              (buffer-substring-no-properties (car pos)
                                                              (cdr pos)))))))
        (if reply
            (let ((lines (split-string reply "\n" t)))
              (cl-remove-if
               #'null
               (mapcar (lambda (line)
                         (if (string-match "\\(.*?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                                           line)
                             (list :path (match-string 1 line)
                                   :start-line (string-to-number (match-string 2 line))
                                   ;; ;; GHC uses 1-based columns.
                                   :start-col (1- (string-to-number (match-string 3 line)))
                                   :end-line (string-to-number (match-string 4 line))
                                   ;; GHC uses 1-based columns.
                                   :end-col (1- (string-to-number (match-string 5 line))))
                           (error (propertize line 'face 'compilation-error))))
                       lines)))
          (error (propertize "No reply. Is :uses supported?"
                             'face 'compilation-error)))))))

(defun haskell-command-echo-or-present (msg)
  "Present message in some manner depending on configuration.
If variable `haskell-process-use-presentation-mode' is NIL it will output
modified message MSG to echo area."
  (if haskell-process-use-presentation-mode
      (let ((session (haskell-process-session (haskell-interactive-process))))
        (haskell-presentation-present session msg))
    (let ((m (haskell-utils-reduce-string msg)))
      (message "%s" m))))

(defun haskell-command-capture-expr-bounds ()
  "Capture position bounds of expression at point.
If there is an active region then it returns region
bounds.  Otherwise it uses `haskell-spanable-pos-at-point` to
capture identifier bounds.  If latter function returns NIL this function
will return cons cell where min and max positions both are equal
to point."
  (or (when (region-active-p)
        (cons (region-beginning)
              (region-end)))
      (haskell-spanable-pos-at-point)
      (cons (point) (point))))

(defun haskell-command-insert-type-signature (signature)
  "Insert type signature.
In case of active region is present, wrap it by parentheses and
append SIGNATURE to original expression.  Otherwise tries to
carefully insert SIGNATURE above identifier at point.  Removes
newlines and extra whitespace in signature before insertion."
  (let* ((ident-pos (or (haskell-ident-pos-at-point)
                        (cons (point) (point))))
         (min-pos (car ident-pos))
         (sig (haskell-utils-reduce-string signature)))
    (save-excursion
      (goto-char min-pos)
      (let ((col (current-column)))
        (insert sig "\n")
        (indent-to col)))))

(provide 'haskell-commands)
;;; haskell-commands.el ends here
