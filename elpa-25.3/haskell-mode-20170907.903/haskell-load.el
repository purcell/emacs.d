;;; haskell-load.el --- Compiling and loading modules in the GHCi process -*- lexical-binding: t -*-

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

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'haskell-mode)
(require 'haskell-process)
(require 'haskell-interactive-mode)
(require 'haskell-modules)
(require 'haskell-commands)
(require 'haskell-session)
(require 'haskell-string)

(defun haskell-process-look-config-changes (session)
  "Check whether a cabal configuration file has changed.
Restarts the SESSION's process if that is the case."
  (let ((current-checksum (haskell-session-get session 'cabal-checksum))
        (new-checksum (haskell-cabal-compute-checksum
                       (haskell-session-get session 'cabal-dir))))
    (when (not (string= current-checksum new-checksum))
      (haskell-interactive-mode-echo
       session
       (format "Cabal file changed: %s" new-checksum))
      (haskell-session-set-cabal-checksum
       session
       (haskell-session-get session 'cabal-dir))
      (haskell-mode-toggle-interactive-prompt-state)
      (unwind-protect
          (unless
              (and haskell-process-prompt-restart-on-cabal-change
                   (not
                    (y-or-n-p "Cabal file changed. Restart GHCi process? ")))
            (haskell-process-start (haskell-interactive-session)))
        (haskell-mode-toggle-interactive-prompt-state t)))))

(defun haskell-process-live-build (process buffer echo-in-repl)
  "Show live updates for loading files."
  (cond
   ((haskell-process-consume
     process
     (concat "\\[[ ]*\\([0-9]+\\) of \\([0-9]+\\)\\]"
             " Compiling \\([^ ]+\\)[ ]+"
             "( \\([^ ]+\\), \\([^ ]+\\) )[^\r\n]*[\r\n]+"))
    (haskell-process-echo-load-message process buffer echo-in-repl nil)
    t)
   ((haskell-process-consume
     process
     (concat "\\[[ ]*\\([0-9]+\\) of \\([0-9]+\\)\\]"
             " Compiling \\[TH\\] \\([^ ]+\\)[ ]+"
             "( \\([^ ]+\\), \\([^ ]+\\) )[^\r\n]*[\r\n]+"))
    (haskell-process-echo-load-message process buffer echo-in-repl t)
    t)
   ((haskell-process-consume
     process
     "Loading package \\([^ ]+\\) ... linking ... done.\n")
    (haskell-mode-message-line
     (format "Loading: %s"
             (match-string 1 buffer)))
    t)
   ((haskell-process-consume
     process
     "^Preprocessing executables for \\(.+?\\)\\.\\.\\.")
    (let ((msg (format "Preprocessing: %s" (match-string 1 buffer))))
      (haskell-interactive-mode-echo (haskell-process-session process) msg)
      (haskell-mode-message-line msg)))
   ((haskell-process-consume process "Linking \\(.+?\\) \\.\\.\\.")
    (let ((msg (format "Linking: %s" (match-string 1 buffer))))
      (haskell-interactive-mode-echo (haskell-process-session process) msg)
      (haskell-mode-message-line msg)))
   ((haskell-process-consume process "\nBuilding \\(.+?\\)\\.\\.\\.")
    (let ((msg (format "Building: %s" (match-string 1 buffer))))
      (haskell-interactive-mode-echo (haskell-process-session process) msg)
      (haskell-mode-message-line msg)))
   ((string-match "Collecting type info for [[:digit:]]+ module(s) \\.\\.\\."
                  (haskell-process-response process)
                  (haskell-process-response-cursor process))
    (haskell-mode-message-line (match-string 0 buffer))
    ;; Do not consume "Ok, modules loaded" that goes before
    ;; "Collecting type info...", just exit.
    nil)))

(defun haskell-process-load-complete (session process buffer reload module-buffer &optional cont)
  "Handle the complete loading response. BUFFER is the string of
text being sent over the process pipe. MODULE-BUFFER is the
actual Emacs buffer of the module being loaded."
  (when (get-buffer (format "*%s:splices*" (haskell-session-name session)))
    (with-current-buffer (haskell-interactive-mode-splices-buffer session)
      (erase-buffer)))
  (let* ((ok (cond
              ((haskell-process-consume
                process
                "Ok, modules loaded: \\(.+\\)\\.$")
               t)
              ((haskell-process-consume
                process
                "Failed, modules loaded: \\(.+\\)\\.$")
               nil)
              (t
               (error (message "Unexpected response from haskell process.")))))
         (modules (haskell-process-extract-modules buffer))
         (cursor (haskell-process-response-cursor process))
         (warning-count 0))
    (haskell-process-set-response-cursor process 0)
    (haskell-check-remove-overlays module-buffer)
    (while
        (haskell-process-errors-warnings module-buffer session process buffer)
      (setq warning-count (1+ warning-count)))
    (haskell-process-set-response-cursor process cursor)
    (if (and (not reload)
             haskell-process-reload-with-fbytecode)
        (haskell-process-reload-with-fbytecode process module-buffer)
      (haskell-process-import-modules process (car modules)))
    (if ok
        (haskell-mode-message-line (if reload "Reloaded OK." "OK."))
      (haskell-interactive-mode-compile-error session "Compilation failed."))
    (when cont
      (condition-case-unless-debug e
          (funcall cont ok)
        (error (message "%S" e))
        (quit nil)))))

(defun haskell-process-suggest-imports (session file modules ident)
  "Suggest add missed imports to file.
Asks user to add to SESSION's FILE missed import.  MODULES is a
list of modules where missed IDENT was found."
  (cl-assert session)
  (cl-assert file)
  (cl-assert ident)
  (haskell-mode-toggle-interactive-prompt-state)
  (unwind-protect
      (let* ((process (haskell-session-process session))
             (suggested-already (haskell-process-suggested-imports process))
             (module
              (cond
               ((> (length modules) 1)
                (when (y-or-n-p
                       (format
                        "Identifier `%s' not in scope, choose module to import?"
                        ident))
                  (haskell-complete-module-read "Module: " modules)))
               ((= (length modules) 1)
                (let ((module (car modules)))
                  (unless (member module suggested-already)
                    (haskell-process-set-suggested-imports
                     process
                     (cons module suggested-already))
                    (when (y-or-n-p
                           (format "Identifier `%s' not in scope, import `%s'?"
                                   ident
                                   module))
                      module)))))))
        (when module
          (haskell-process-find-file session file)
          (haskell-add-import module)))
    (haskell-mode-toggle-interactive-prompt-state t)))

(defun haskell-process-trigger-suggestions (session msg file line)
  "Trigger prompting to add any extension suggestions."
  (cond ((let ((case-fold-search nil))
           (or
            (and (string-match " -X\\([A-Z][A-Za-z]+\\)" msg)
                 (not (string-match "\\([A-Z][A-Za-z]+\\) is deprecated" msg)))
            (string-match "Use \\([A-Z][A-Za-z]+\\) to permit this" msg)
            (string-match "Use \\([A-Z][A-Za-z]+\\) to allow" msg)
            (string-match "Use \\([A-Z][A-Za-z]+\\) to enable" msg)
            (string-match
             "Use \\([A-Z][A-Za-z]+\\) if you want to disable this"
             msg)
            (string-match "use \\([A-Z][A-Za-z]+\\)" msg)
            (string-match "You need \\([A-Z][A-Za-z]+\\)" msg)))
         (when haskell-process-suggest-language-pragmas
           (haskell-process-suggest-pragma
            session
            "LANGUAGE"
            (match-string 1 msg)
            file)))
        ((string-match
          " The \\(qualified \\)?import of[ ][‘`‛]\\([^ ]+\\)['’] is redundant"
          msg)
         (when haskell-process-suggest-remove-import-lines
           (haskell-process-suggest-remove-import
            session
            file
            (match-string 2 msg)
            line)))
        ((string-match "[Ww]arning: orphan instance: " msg)
         (when haskell-process-suggest-no-warn-orphans
           (haskell-process-suggest-pragma
            session
            "OPTIONS" "-fno-warn-orphans"
            file)))
        ((or (string-match "against inferred type [‘`‛]\\[Char\\]['’]" msg)
             (string-match "with actual type [‘`‛]\\[Char\\]['’]" msg))
         (when haskell-process-suggest-overloaded-strings
           (haskell-process-suggest-pragma
            session
            "LANGUAGE" "OverloadedStrings"
            file)))
        ((string-match "^Not in scope: .*[‘`‛]\\(.+\\)['’]$" msg)
         (let* ((match1 (match-string 1 msg))
                (ident (if (string-match "^[A-Za-z0-9_'.]+\\.\\(.+\\)$" match1)
                           ;; Skip qualification.
                           (match-string 1 match1)
                         match1)))
           (when haskell-process-suggest-hoogle-imports
             (let ((modules (haskell-process-hoogle-ident ident)))
               (haskell-process-suggest-imports session file modules ident)))
           (when haskell-process-suggest-haskell-docs-imports
             (let ((modules (haskell-process-haskell-docs-ident ident)))
               (haskell-process-suggest-imports session file modules ident)))
           (when haskell-process-suggest-hayoo-imports
             (let ((modules (haskell-process-hayoo-ident ident)))
               (haskell-process-suggest-imports session file modules ident)))))
        ((string-match "^[ ]+It is a member of the hidden package [‘`‛]\\([^@\r\n]+\\).*['’].$" msg)
         (when haskell-process-suggest-add-package
           (haskell-process-suggest-add-package session msg)))))

(defun haskell-process-do-cabal (command)
  "Run a Cabal command."
  (let ((process (ignore-errors
                   (haskell-interactive-process))))
    (cond
     ((or (eq process nil)
          (let ((child (haskell-process-process process)))
            (not (equal 'run (process-status child)))))
      (message "Process is not running, so running directly.")
      (shell-command (concat "cabal " command)
                     (get-buffer-create "*haskell-process-log*")
                     (get-buffer-create "*haskell-process-log*"))
      (switch-to-buffer-other-window (get-buffer "*haskell-process-log*")))
     (t (haskell-process-queue-command
         process
         (make-haskell-command
          :state (list (haskell-interactive-session) process command 0)
          :go
          (lambda (state)
            (haskell-process-send-string
             (cadr state)
             (format haskell-process-do-cabal-format-string
                     (haskell-session-cabal-dir (car state))
                     (format "%s %s"
                             (cl-ecase (haskell-process-type)
                               ('ghci haskell-process-path-cabal)
                               ('cabal-repl haskell-process-path-cabal)
                               ('cabal-new-repl haskell-process-path-cabal)
                               ('cabal-ghci haskell-process-path-cabal)
                               ('stack-ghci haskell-process-path-stack))
                             (cl-caddr state)))))
          :live
          (lambda (state buffer)
            (let ((cmd (replace-regexp-in-string "^\\([a-z]+\\).*"
                                                 "\\1"
                                                 (cl-caddr state))))
              (cond ((or (string= cmd "build")
                         (string= cmd "install"))
                     (haskell-process-live-build (cadr state) buffer t))
                    (t
                     (haskell-process-cabal-live state buffer)))))
          :complete
          (lambda (state response)
            (let* ((process (cadr state))
                   (session (haskell-process-session process))
                   (message-count 0)
                   (cursor (haskell-process-response-cursor process)))
              ;; XXX: what the hell about the rampant code duplication?
              (haskell-process-set-response-cursor process 0)
              (while (haskell-process-errors-warnings nil session process response)
                (setq message-count (1+ message-count)))
              (haskell-process-set-response-cursor process cursor)
              (let ((msg (format "Complete: cabal %s (%s compiler messages)"
                                 (cl-caddr state)
                                 message-count)))
                (haskell-interactive-mode-echo session msg)
                (when (= message-count 0)
                  (haskell-interactive-mode-echo
                   session
                   "No compiler messages, dumping complete output:")
                  (haskell-interactive-mode-echo session response))
                (haskell-mode-message-line msg)
                (when (and haskell-notify-p
                           (fboundp 'notifications-notify))
                  (notifications-notify
                   :title (format "*%s*" (haskell-session-name (car state)))
                   :body msg
                   :app-name (cl-ecase (haskell-process-type)
                               ('ghci haskell-process-path-cabal)
                               ('cabal-repl haskell-process-path-cabal)
                               ('cabal-new-repl haskell-process-path-cabal)
                               ('cabal-ghci haskell-process-path-cabal)
                               ('stack-ghci haskell-process-path-stack))
                   :app-icon haskell-process-logo)))))))))))

(defun haskell-process-echo-load-message (process buffer echo-in-repl th)
  "Echo a load message."
  (let ((session (haskell-process-session process))
        (module-name (match-string 3 buffer))
        (file-name (match-string 4 buffer)))
    (haskell-interactive-show-load-message
     session
     'compiling
     module-name
     (haskell-session-strip-dir session file-name)
     echo-in-repl
     th)))

(defun haskell-process-extract-modules (buffer)
  "Extract the modules from the process buffer."
  (let* ((modules-string (match-string 1 buffer))
         (modules (split-string modules-string ", ")))
    (cons modules modules-string)))

;;;###autoload
(defface haskell-error-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "#dc322f"))
    (t
     :inherit error))
  "Face used for marking error lines."
  :group 'haskell-mode)

;;;###autoload
(defface haskell-warning-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "#b58900"))
    (t
     :inherit warning))
  "Face used for marking warning lines."
  :group 'haskell-mode)

;;;###autoload
(defface haskell-hole-face
  '((((supports :underline (:style wave)))
     :underline (:style wave :color "#6c71c4"))
    (t
     :inherit warning))
  "Face used for marking hole lines."
  :group 'haskell-mode)

(defvar haskell-check-error-fringe   (propertize "!" 'display '(left-fringe exclamation-mark)))
(defvar haskell-check-warning-fringe (propertize "?" 'display '(left-fringe question-mark)))
(defvar haskell-check-hole-fringe    (propertize "_" 'display '(left-fringe horizontal-bar)))

(defun haskell-check-overlay-p (ovl)
  (overlay-get ovl 'haskell-check))

(defun haskell-check-filter-overlays (xs)
  (cl-remove-if-not 'haskell-check-overlay-p xs))

(defun haskell-check-remove-overlays (buffer)
  (with-current-buffer buffer
    (remove-overlays (point-min) (point-max) 'haskell-check t)))

(defmacro with-overlay-properties (proplist ovl &rest body)
  "Evaluate BODY with names in PROPLIST bound to the values of
correspondingly-named overlay properties of OVL."
  (let ((ovlvar (cl-gensym "OVL-")))
    `(let* ((,ovlvar ,ovl)
            ,@(mapcar (lambda (p) `(,p (overlay-get ,ovlvar ',p))) proplist))
       ,@body)))

(defun overlay-start> (o1 o2)
  (> (overlay-start o1) (overlay-start o2)))
(defun overlay-start< (o1 o2)
  (< (overlay-start o1) (overlay-start o2)))

(defun first-overlay-in-if (test beg end)
  (let ((ovls (cl-remove-if-not test (overlays-in beg end))))
    (cl-first (sort (cl-copy-list ovls) 'overlay-start<))))

(defun last-overlay-in-if (test beg end)
  (let ((ovls (cl-remove-if-not test (overlays-in beg end))))
    (cl-first (sort (cl-copy-list ovls) 'overlay-start>))))

(defun haskell-error-overlay-briefly (ovl)
  (with-overlay-properties
   (haskell-msg haskell-msg-type) ovl
   (cond
    ((not (eq haskell-msg-type 'warning))
     haskell-msg)
    ((string-prefix-p "[Ww]arning:\n    " haskell-msg)
     (cl-subseq haskell-msg 13))
    (t
     (error
      "Invariant failed: a warning message from GHC has unexpected form: %s."
      haskell-msg)))))

(defun haskell-goto-error-overlay (ovl)
  (cond (ovl
         (goto-char (overlay-start ovl))
         (haskell-mode-message-line (haskell-error-overlay-briefly ovl)))
        (t
         (message "No further notes from Haskell compiler."))))

(defun haskell-goto-first-error ()
  (interactive)
  (haskell-goto-error-overlay
   (first-overlay-in-if 'haskell-check-overlay-p
                        (buffer-end 0) (buffer-end 1))))

(defun haskell-goto-prev-error ()
  (interactive)
  (haskell-goto-error-overlay
   (let ((ovl-at
          (cl-first (haskell-check-filter-overlays (overlays-at (point))))))
     (or (last-overlay-in-if 'haskell-check-overlay-p
                             (point-min)
                             (if ovl-at (overlay-start ovl-at) (point)))
         ovl-at))))

(defun haskell-goto-next-error ()
  (interactive)
  (haskell-goto-error-overlay
   (let ((ovl-at
          (cl-first (haskell-check-filter-overlays (overlays-at (point))))))
     (or (first-overlay-in-if
          'haskell-check-overlay-p
          (if ovl-at (overlay-end ovl-at) (point)) (point-max))
         ovl-at))))

(defun haskell-check-paint-overlay
    (buffer error-from-this-file-p line msg file type hole coln)
  (with-current-buffer buffer
    (let (beg end)
      (goto-char (point-min))
      ;; XXX: we can avoid excess buffer walking by relying on the maybe-fact
      ;;      that GHC sorts error messages by line number, maybe.
      (cond
       (error-from-this-file-p
        (forward-line (1- line))
        (forward-char (1- coln))
        (setq beg (point))
        (if (eq type 'hole)
            (forward-char (length hole))
          (skip-chars-forward "^[:space:]" (line-end-position)))
        (setq end (point)))
       (t
        (setq beg (point))
        (forward-line)
        (setq end (point))))
      (let ((ovl (make-overlay beg end)))
        (overlay-put ovl 'haskell-check t)
        (overlay-put ovl 'haskell-file file)
        (overlay-put ovl 'haskell-msg msg)
        (overlay-put ovl 'haskell-msg-type type)
        (overlay-put ovl 'help-echo msg)
        (overlay-put ovl 'haskell-hole hole)
        (cl-destructuring-bind
            (face fringe)
            (cl-case type
              (warning
               (list 'haskell-warning-face haskell-check-warning-fringe))
              (hole
               (list 'haskell-hole-face    haskell-check-hole-fringe))
              (error
               (list 'haskell-error-face   haskell-check-error-fringe)))
          (overlay-put ovl 'before-string fringe)
          (overlay-put ovl 'face face))))))

(defun haskell-process-errors-warnings
    (module-buffer session process buffer &optional return-only)
  "Trigger handling type errors or warnings.
Either prints the messages in the interactive buffer or if CONT
is specified, passes the error onto that.

When MODULE-BUFFER is non-NIL, paint error overlays."
  (save-excursion
    (cond
     ((haskell-process-consume
       process
       "\\(Module imports form a cycle:[ \n]+module [^ ]+ ([^)]+)[[:unibyte:][:nonascii:]]+?\\)\nFailed")
      (let ((err (match-string 1 buffer)))
        (if (string-match "module [`'‘‛]\\([^ ]+\\)['’`] (\\([^)]+\\))" err)
            (let* ((default-directory (haskell-session-current-dir session))
                   (module (match-string 1 err))
                   (file (match-string 2 err))
                   (relative-file-name (file-relative-name file)))
              (unless return-only
                (haskell-interactive-show-load-message
                 session
                 'import-cycle
                 module
                 relative-file-name
                 nil
                 nil)
                (haskell-interactive-mode-compile-error
                 session
                 (format "%s:1:0: %s"
                         relative-file-name
                         err)))
              (list :file file :line 1 :col 0 :msg err :type 'error))
          t)))
     ((haskell-process-consume
       process
       (concat "[\r\n]\\([A-Z]?:?[^ \r\n:][^:\n\r]+\\):\\([0-9()-:]+\\):"
               "[ \n\r]+\\([[:unibyte:][:nonascii:]]+?\\)\n[^ ]"))
      (haskell-process-set-response-cursor
       process
       (- (haskell-process-response-cursor process) 1))
      (let* ((buffer (haskell-process-response process))
             (file (match-string 1 buffer))
             (location-raw (match-string 2 buffer))
             (error-msg (match-string 3 buffer))
             (type (cond ((string-match "^[Ww]arning:" error-msg)  'warning)
                         ((string-match "^Splicing " error-msg) 'splice)
                         (t                                     'error)))
             (critical (not (eq type 'warning)))
             ;; XXX: extract hole information, pass down to
             ;; `haskell-check-paint-overlay'
             (final-msg (format "%s:%s: %s"
                                (haskell-session-strip-dir session file)
                                location-raw
                                error-msg))
             (location (haskell-process-parse-error
                        (concat file ":" location-raw ": x")))
             (line (plist-get location :line))
             (col1 (plist-get location :col)))
        (when module-buffer
          (haskell-check-paint-overlay
           module-buffer
           (string= (file-truename (buffer-file-name module-buffer))
                    (file-truename file))
           line error-msg file type nil col1))
        (if return-only
            (list :file file :line line :col col1 :msg error-msg :type type)
          (progn (funcall (cl-case type
                            (warning  'haskell-interactive-mode-compile-warning)
                            (splice   'haskell-interactive-mode-compile-splice)
                            (error    'haskell-interactive-mode-compile-error))
                          session final-msg)
                 (when critical
                   (haskell-mode-message-line final-msg))
                 (haskell-process-trigger-suggestions
                  session
                  error-msg
                  file
                  line)
                 t)))))))

(defun haskell-interactive-show-load-message (session type module-name file-name echo th)
  "Show the '(Compiling|Loading) X' message."
  (let ((msg (concat
              (cl-ecase type
                ('compiling
                 (if haskell-interactive-mode-include-file-name
                     (format "Compiling: %s (%s)" module-name file-name)
                   (format "Compiling: %s" module-name)))
                ('loading (format "Loading: %s" module-name))
                ('import-cycle
                 (format "Module has an import cycle: %s" module-name)))
              (if th " [TH]" ""))))
    (haskell-mode-message-line msg)
    (when haskell-interactive-mode-delete-superseded-errors
      (haskell-interactive-mode-delete-compile-messages session file-name))
    (when echo
      (haskell-interactive-mode-echo session msg))))

;;;###autoload
(defun haskell-process-reload-devel-main ()
  "Reload the module `DevelMain' and then run `DevelMain.update'.

This is for doing live update of the code of servers or GUI
applications.  Put your development version of the program in
`DevelMain', and define `update' to auto-start the program on a
new thread, and use the `foreign-store' package to access the
running context across :load/:reloads in GHCi."
  (interactive)
  (haskell-mode-toggle-interactive-prompt-state)
  (unwind-protect
      (with-current-buffer
          (or (get-buffer "DevelMain.hs")
              (if (y-or-n-p
                   "You need to open a buffer named DevelMain.hs. Find now?")
                  (ido-find-file)
                (error "No DevelMain.hs buffer.")))
        (let ((session (haskell-interactive-session)))
          (let ((process (haskell-interactive-process)))
            (haskell-process-queue-command
             process
             (make-haskell-command
              :state (list :session session
                           :process process
                           :buffer (current-buffer))
              :go (lambda (state)
                    (haskell-process-send-string (plist-get state ':process)
                                                 ":l DevelMain"))
              :live (lambda (state buffer)
                      (haskell-process-live-build (plist-get state ':process)
                                                  buffer
                                                  nil))
              :complete (lambda (state response)
                          (haskell-process-load-complete
                           (plist-get state ':session)
                           (plist-get state ':process)
                           response
                           nil
                           (plist-get state ':buffer)
                           (lambda (ok)
                             (when ok
                               (haskell-process-queue-without-filters
                                (haskell-interactive-process)
                                "DevelMain.update")
                               (message "DevelMain updated."))))))))))
    (haskell-mode-toggle-interactive-prompt-state t)))

(provide 'haskell-load)
;;; haskell-load.el ends here
