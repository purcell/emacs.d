;;; haskell.el --- Top-level Haskell package -*- lexical-binding: t -*-

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

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'haskell-mode)
(require 'haskell-hoogle)
(require 'haskell-process)
(require 'haskell-debug)
(require 'haskell-interactive-mode)
(require 'haskell-repl)
(require 'haskell-load)
(require 'haskell-commands)
(require 'haskell-modules)
(require 'haskell-string)
(require 'haskell-completions)
(require 'haskell-utils)
(require 'haskell-customize)

(defvar interactive-haskell-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key map (kbd "C-c C-r") 'haskell-process-reload)
    (define-key map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
    (define-key map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key map (kbd "C-c C-c") 'haskell-process-cabal-build)
    (define-key map (kbd "C-c v c") 'haskell-cabal-visit-file)
    (define-key map (kbd "C-c C-x") 'haskell-process-cabal)
    (define-key map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key map (kbd "C-c C-z") 'haskell-interactive-switch)
    map)
  "Keymap for using `interactive-haskell-mode'.")

;;;###autoload
(define-minor-mode interactive-haskell-mode
  "Minor mode for enabling haskell-process interaction."
  :lighter " Interactive"
  :keymap interactive-haskell-mode-map
  (add-hook 'completion-at-point-functions
            #'haskell-completions-sync-repl-completion-at-point
            nil
            t))

(make-obsolete 'haskell-process-completions-at-point
               'haskell-completions-sync-repl-completion-at-point
               "June 19, 2015")

(defun haskell-process-completions-at-point ()
  "A `completion-at-point' function using the current haskell process."
  (when (haskell-session-maybe)
    (let ((process (haskell-process))
          symbol-bounds)
      (cond
       ;; ghci can complete module names, but it needs the "import "
       ;; string at the beginning
       ((looking-back (rx line-start
                          "import" (1+ space)
                          (? "qualified" (1+ space))
                          (group (? (char upper) ; modid
                                    (* (char alnum ?' ?.)))))
                      (line-beginning-position))
        (let ((text (match-string-no-properties 0))
              (start (match-beginning 1))
              (end (match-end 1)))
          (list start end
                (haskell-process-get-repl-completions process text))))
       ;; Complete OPTIONS, a completion list comes from variable
       ;; `haskell-ghc-supported-options'
       ((and (nth 4 (syntax-ppss))
           (save-excursion
             (let ((p (point)))
               (and (search-backward "{-#" nil t)
                  (search-forward-regexp "\\_<OPTIONS\\(?:_GHC\\)?\\_>" p t))))
           (looking-back
            (rx symbol-start "-" (* (char alnum ?-)))
            (line-beginning-position)))
        (list (match-beginning 0) (match-end 0) haskell-ghc-supported-options))
       ;; Complete LANGUAGE, a list of completions comes from variable
       ;; `haskell-ghc-supported-extensions'
       ((and (nth 4 (syntax-ppss))
           (save-excursion
             (let ((p (point)))
               (and (search-backward "{-#" nil t)
                  (search-forward-regexp "\\_<LANGUAGE\\_>" p t))))
           (setq symbol-bounds (bounds-of-thing-at-point 'symbol)))
        (list (car symbol-bounds) (cdr symbol-bounds)
              haskell-ghc-supported-extensions))
       ((setq symbol-bounds (haskell-ident-pos-at-point))
        (cl-destructuring-bind (start . end) symbol-bounds
          (list start end
                (haskell-process-get-repl-completions
                 process (buffer-substring-no-properties start end)))))))))

;;;###autoload
(defun haskell-interactive-mode-return ()
  "Handle the return key."
  (interactive)
  (cond
   ;; At a compile message, jump to the location of the error in the
   ;; source.
   ((haskell-interactive-at-compile-message)
    (next-error-internal))
   ;; At the input prompt, handle the expression in the usual way.
   ((haskell-interactive-at-prompt)
    (haskell-interactive-handle-expr))
   ;; At any other location in the buffer, copy the line to the
   ;; current prompt.
   (t
    (haskell-interactive-copy-to-prompt))))

;;;###autoload
(defun haskell-session-kill (&optional leave-interactive-buffer)
  "Kill the session process and buffer, delete the session.
0. Prompt to kill all associated buffers.
1. Kill the process.
2. Kill the interactive buffer unless LEAVE-INTERACTIVE-BUFFER is not given.
3. Walk through all the related buffers and set their haskell-session to nil.
4. Remove the session from the sessions list."
  (interactive)
  (haskell-mode-toggle-interactive-prompt-state)
  (unwind-protect
      (let* ((session (haskell-session))
             (name (haskell-session-name session))
             (also-kill-buffers
              (and haskell-ask-also-kill-buffers
                   (y-or-n-p
                    (format "Killing `%s'. Also kill all associated buffers?"
                            name)))))
        (haskell-kill-session-process session)
        (unless leave-interactive-buffer
          (kill-buffer (haskell-session-interactive-buffer session)))
        (cl-loop for buffer in (buffer-list)
                 do (with-current-buffer buffer
                      (when (and (boundp 'haskell-session)
                                 (string= (haskell-session-name haskell-session)
                                          name))
                        (setq haskell-session nil)
                        (when also-kill-buffers
                          (kill-buffer)))))
        (setq haskell-sessions
              (cl-remove-if (lambda (session)
                              (string= (haskell-session-name session)
                                       name))
                            haskell-sessions)))
    (haskell-mode-toggle-interactive-prompt-state t)))

;;;###autoload
(defun haskell-interactive-kill ()
  "Kill the buffer and (maybe) the session."
  (interactive)
  (when (eq major-mode 'haskell-interactive-mode)
    (haskell-mode-toggle-interactive-prompt-state)
    (unwind-protect
        (when (and (boundp 'haskell-session)
                   haskell-session
                   (y-or-n-p "Kill the whole session?"))
          (haskell-session-kill t)))
    (haskell-mode-toggle-interactive-prompt-state t)))

(defun haskell-session-make (name)
  "Make a Haskell session."
  (when (haskell-session-lookup name)
    (error "Session of name %s already exists!" name))
  (let ((session (setq haskell-session
                       (list (cons 'name name)))))
    (add-to-list 'haskell-sessions session)
    (haskell-process-start session)
    session))

(defun haskell-session-new-assume-from-cabal ()
  "Prompt to create a new project based on a guess from the nearest Cabal file.
If `haskell-process-load-or-reload-prompt' is nil, accept `default'."
  (let ((name (haskell-session-default-name)))
    (unless (haskell-session-lookup name)
      (haskell-mode-toggle-interactive-prompt-state)
      (unwind-protect
          (if (or (not haskell-process-load-or-reload-prompt)
                  (y-or-n-p (format "Start a new project named “%s”? " name)))
              (haskell-session-make name))
        (haskell-mode-toggle-interactive-prompt-state t)))))

;;;###autoload
(defun haskell-session ()
  "Get the Haskell session, prompt if there isn't one or fail."
  (or (haskell-session-maybe)
      (haskell-session-assign
       (or (haskell-session-from-buffer)
           (haskell-session-new-assume-from-cabal)
           (haskell-session-choose)
           (haskell-session-new)))))

;;;###autoload
(defun haskell-interactive-switch ()
  "Switch to the interactive mode for this session."
  (interactive)
  (let ((initial-buffer (current-buffer))
        (buffer (haskell-session-interactive-buffer (haskell-session))))
    (with-current-buffer buffer
      (setq haskell-interactive-previous-buffer initial-buffer))
    (unless (eq buffer (window-buffer))
      (switch-to-buffer-other-window buffer))))

(defun haskell-session-new ()
  "Make a new session."
  (let ((name (read-from-minibuffer "Project name: " (haskell-session-default-name))))
    (when (not (string= name ""))
      (let ((session (haskell-session-lookup name)))
        (haskell-mode-toggle-interactive-prompt-state)
        (unwind-protect
            (if session
                (when
                    (y-or-n-p
                     (format "Session %s already exists. Use it?" name))
                  session)
              (haskell-session-make name)))
        (haskell-mode-toggle-interactive-prompt-state t)))))

;;;###autoload
(defun haskell-session-change ()
  "Change the session for the current buffer."
  (interactive)
  (haskell-session-assign (or (haskell-session-new-assume-from-cabal)
                              (haskell-session-choose)
                              (haskell-session-new))))

(defun haskell-process-prompt-restart (process)
  "Prompt to restart the died PROCESS."
  (let ((process-name (haskell-process-name process))
        (cursor-in-echo-area t))
    (if haskell-process-suggest-restart
        (progn
          (haskell-mode-toggle-interactive-prompt-state)
          (unwind-protect
              (cond
               ((string-match "You need to re-run the 'configure' command."
                              (haskell-process-response process))
                (cl-case (read-char-choice
                          (concat
                           "The Haskell process ended. Cabal wants you to run "
                           (propertize "cabal configure"
                                       'face
                                       'font-lock-keyword-face)
                           " because there is a version mismatch. Re-configure (y, n, l: view log)?"
                           "\n\n"
                           "Cabal said:\n\n"
                           (propertize (haskell-process-response process)
                                       'face
                                       'font-lock-comment-face))
                          '(?l ?n ?y))
                  (?y (let ((default-directory
                              (haskell-session-cabal-dir
                               (haskell-process-session process))))
                        (message "%s"
                                 (shell-command-to-string "cabal configure"))))
                  (?l (let* ((response (haskell-process-response process))
                             (buffer (get-buffer "*haskell-process-log*")))
                        (if buffer
                            (switch-to-buffer buffer)
                          (progn (switch-to-buffer
                                  (get-buffer-create "*haskell-process-log*"))
                                 (insert response)))))
                  (?n)))
               (t
                (cl-case (read-char-choice
                          (propertize
                           (format "The Haskell process `%s' has died. Restart? (y, n, l: show process log) "
                                   process-name)
                           'face
                           'minibuffer-prompt)
                          '(?l ?n ?y))
                  (?y (haskell-process-start (haskell-process-session process)))
                  (?l (let* ((response (haskell-process-response process))
                             (buffer (get-buffer "*haskell-process-log*")))
                        (if buffer
                            (switch-to-buffer buffer)
                          (progn (switch-to-buffer
                                  (get-buffer-create "*haskell-process-log*"))
                                 (insert response)))))
                  (?n))))
            ;; unwind
            (haskell-mode-toggle-interactive-prompt-state t)))
      (message "The Haskell process `%s' is dearly departed." process-name))))

(defun haskell-process ()
  "Get the current process from the current session."
  (haskell-session-process (haskell-session)))

;;;###autoload
(defun haskell-kill-session-process (&optional session)
  "Kill the process."
  (interactive)
  (let* ((session (or session (haskell-session)))
         (existing-process (get-process (haskell-session-name session))))
    (when (processp existing-process)
      (haskell-interactive-mode-echo session "Killing process ...")
      (haskell-process-set (haskell-session-process session) 'is-restarting t)
      (delete-process existing-process))))

;;;###autoload
(defun haskell-interactive-mode-visit-error ()
  "Visit the buffer of the current (or last) error message."
  (interactive)
  (with-current-buffer (haskell-session-interactive-buffer (haskell-session))
    (if (progn (goto-char (line-beginning-position))
               (looking-at haskell-interactive-mode-error-regexp))
        (progn (forward-line -1)
               (haskell-interactive-jump-to-error-line))
      (progn (goto-char (point-max))
             (haskell-interactive-mode-error-backward)
             (haskell-interactive-jump-to-error-line)))))

(defvar xref-prompt-for-identifier nil)

;;;###autoload
(defun haskell-mode-jump-to-tag (&optional next-p)
  "Jump to the tag of the given identifier.

Give optional NEXT-P parameter to override value of
`xref-prompt-for-identifier' during definition search."
  (interactive "P")
  (let ((ident (haskell-string-drop-qualifier (haskell-ident-at-point)))
        (tags-file-dir (haskell-cabal--find-tags-dir))
        (tags-revert-without-query t))
    (when (and ident
               (not (string= "" (haskell-string-trim ident)))
               tags-file-dir)
      (let ((tags-file-name (concat tags-file-dir "TAGS")))
        (cond ((file-exists-p tags-file-name)
               (let ((xref-prompt-for-identifier next-p))
                 (xref-find-definitions ident)))
              (t (haskell-mode-generate-tags ident)))))))

;;;###autoload
(defun haskell-mode-after-save-handler ()
  "Function that will be called after buffer's saving."
  (when haskell-tags-on-save
    (ignore-errors (haskell-mode-generate-tags))))

;;;###autoload
(defun haskell-mode-tag-find (&optional _next-p)
  "The tag find function, specific for the particular session."
  (interactive "P")
  (cond
   ((elt (syntax-ppss) 3) ;; Inside a string
    (haskell-mode-jump-to-filename-in-string))
   (t (call-interactively 'haskell-mode-jump-to-tag))))

(defun haskell-mode-jump-to-filename-in-string ()
  "Jump to the filename in the current string."
  (let* ((string (save-excursion
                   (buffer-substring-no-properties
                    (1+ (search-backward-regexp "\"" (line-beginning-position) nil 1))
                    (1- (progn (forward-char 1)
                               (search-forward-regexp "\"" (line-end-position) nil 1))))))
         (fp (expand-file-name string
                               (haskell-session-cabal-dir (haskell-session)))))
    (find-file
     (read-file-name
      ""
      fp
      fp))))

;;;###autoload
(defun haskell-interactive-bring ()
  "Bring up the interactive mode for this session."
  (interactive)
  (let* ((session (haskell-session))
         (buffer (haskell-session-interactive-buffer session)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun haskell-process-load-file ()
  "Load the current buffer file."
  (interactive)
  (save-buffer)
  (haskell-interactive-mode-reset-error (haskell-session))
  (haskell-process-file-loadish (format "load \"%s\"" (replace-regexp-in-string
                                                       "\""
                                                       "\\\\\""
                                                       (buffer-file-name)))
                                nil
                                (current-buffer)))

;;;###autoload
(defun haskell-process-reload ()
  "Re-load the current buffer file."
  (interactive)
  (save-buffer)
  (haskell-interactive-mode-reset-error (haskell-session))
  (haskell-process-file-loadish "reload" t (current-buffer)))

;;;###autoload
(defun haskell-process-reload-file () (haskell-process-reload))

(make-obsolete 'haskell-process-reload-file 'haskell-process-reload
               "2015-11-14")

;;;###autoload
(defun haskell-process-load-or-reload (&optional toggle)
  "Load or reload. Universal argument toggles which."
  (interactive "P")
  (if toggle
      (progn (setq haskell-reload-p (not haskell-reload-p))
             (message "%s (No action taken this time)"
                      (if haskell-reload-p
                          "Now running :reload."
                        "Now running :load <buffer-filename>.")))
    (if haskell-reload-p (haskell-process-reload) (haskell-process-load-file))))

(make-obsolete 'haskell-process-load-or-reload 'haskell-process-load-file
               "2015-11-14")

;;;###autoload
(defun haskell-process-cabal-build ()
  "Build the Cabal project."
  (interactive)
  (haskell-process-do-cabal "build")
  (haskell-process-add-cabal-autogen))

;;;###autoload
(defun haskell-process-cabal (p)
  "Prompts for a Cabal command to run."
  (interactive "P")
  (if p
      (haskell-process-do-cabal
       (read-from-minibuffer "Cabal command (e.g. install): "))
    (haskell-process-do-cabal
     (funcall haskell-completing-read-function "Cabal command: "
              (append haskell-cabal-commands
                      (list "build --ghc-options=-fforce-recomp"))))))

(defun haskell-process-file-loadish (command reload-p module-buffer)
  "Run a loading-ish COMMAND that wants to pick up type errors\
and things like that.  RELOAD-P indicates whether the notification
should say 'reloaded' or 'loaded'.  MODULE-BUFFER may be used
for various things, but is optional."
  (let ((session (haskell-session)))
    (haskell-session-current-dir session)
    (when haskell-process-check-cabal-config-on-load
      (haskell-process-look-config-changes session))
    (let ((process (haskell-process)))
      (haskell-process-queue-command
       process
       (make-haskell-command
        :state (list session process command reload-p module-buffer)
        :go (lambda (state)
              (haskell-process-send-string
               (cadr state) (format ":%s" (cl-caddr state))))
        :live (lambda (state buffer)
                (haskell-process-live-build
                 (cadr state) buffer nil))
        :complete (lambda (state response)
                    (haskell-process-load-complete
                     (car state)
                     (cadr state)
                     response
                     (cl-cadddr state)
                     (cl-cadddr (cdr state)))))))))

;;;###autoload
(defun haskell-process-minimal-imports ()
  "Dump minimal imports."
  (interactive)
  (unless (> (save-excursion
               (goto-char (point-min))
               (haskell-navigate-imports-go)
               (point))
             (point))
    (goto-char (point-min))
    (haskell-navigate-imports-go))
  (haskell-process-queue-sync-request (haskell-process)
                                      ":set -ddump-minimal-imports")
  (haskell-process-load-file)
  (insert-file-contents-literally
   (concat (haskell-session-current-dir (haskell-session))
           "/"
           (haskell-guess-module-name-from-file-name (buffer-file-name))
           ".imports")))

(defun haskell-interactive-jump-to-error-line ()
  "Jump to the error line."
  (let ((orig-line (buffer-substring-no-properties (line-beginning-position)
                                                   (line-end-position))))
    (and (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)\\(-[0-9]+\\)?:" orig-line)
         (let* ((file (match-string 1 orig-line))
                (line (match-string 2 orig-line))
                (col (match-string 3 orig-line))
                (session (haskell-interactive-session))
                (cabal-path (haskell-session-cabal-dir session))
                (src-path (haskell-session-current-dir session))
                (cabal-relative-file (expand-file-name file cabal-path))
                (src-relative-file (expand-file-name file src-path)))
           (let ((file (cond ((file-exists-p cabal-relative-file)
                              cabal-relative-file)
                             ((file-exists-p src-relative-file)
                              src-relative-file))))
             (when file
               (other-window 1)
               (find-file file)
               (haskell-interactive-bring)
               (goto-char (point-min))
               (forward-line (1- (string-to-number line)))
               (goto-char (+ (point) (string-to-number col) -1))
               (haskell-mode-message-line orig-line)
               t))))))

(provide 'haskell)
;;; haskell.el ends here
