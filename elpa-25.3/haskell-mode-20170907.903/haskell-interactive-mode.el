;;; haskell-interactive-mode.el --- The interactive Haskell mode -*- lexical-binding: t -*-

;; Copyright © 2011-2012  Chris Done
;;             2016       Arthur Fayzrakhmanov

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

;;; Commentary:

;;; Todo:

;;; Code:

(require 'haskell-mode)
(require 'haskell-compile)
(require 'haskell-process)
(require 'haskell-session)
(require 'haskell-font-lock)
(require 'haskell-presentation-mode)
(require 'haskell-utils)
(require 'haskell-string)
(require 'ansi-color)
(require 'cl-lib)
(require 'etags)

(defvar-local haskell-interactive-mode-history-index 0)

(defvar-local haskell-interactive-mode-history (list))

(defvar-local haskell-interactive-mode-old-prompt-start nil
  "Mark used for the old beginning of the prompt.")

(defun haskell-interactive-prompt-regex ()
  "Generate a regex for searching for any occurence of the prompt\
at the beginning of the line.  This should prevent any
interference with prompts that look like haskell expressions."
  (concat "^" (regexp-quote haskell-interactive-prompt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globals used internally

(declare-function haskell-interactive-kill "haskell")

(defvar haskell-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'haskell-interactive-mode-return)
    (define-key map (kbd "SPC") 'haskell-interactive-mode-space)
    (define-key map (kbd "C-j") 'haskell-interactive-mode-newline-indent)
    (define-key map (kbd "C-a") 'haskell-interactive-mode-beginning)
    (define-key map (kbd "<home>") 'haskell-interactive-mode-beginning)
    (define-key map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
    (define-key map (kbd "C-c C-c") 'haskell-process-interrupt)
    (define-key map (kbd "C-c C-f") 'next-error-follow-minor-mode)
    (define-key map (kbd "C-c C-z") 'haskell-interactive-switch-back)
    (define-key map (kbd "M-p") 'haskell-interactive-mode-history-previous)
    (define-key map (kbd "M-n") 'haskell-interactive-mode-history-next)
    (define-key map (kbd "C-c C-p") 'haskell-interactive-mode-prompt-previous)
    (define-key map (kbd "C-c C-n") 'haskell-interactive-mode-prompt-next)
    (define-key map (kbd "C-<up>") 'haskell-interactive-mode-history-previous)
    (define-key map (kbd "C-<down>") 'haskell-interactive-mode-history-next)
    (define-key map (kbd "TAB") 'haskell-interactive-mode-tab)
    (define-key map (kbd "<C-S-backspace>") 'haskell-interactive-mode-kill-whole-line)
    map)
  "Keymap used in `haskell-interactive-mode'.")

(define-derived-mode haskell-interactive-mode fundamental-mode "Interactive-Haskell"
  "Interactive mode for Haskell.

Key bindings:
\\{haskell-interactive-mode-map}"
  :group 'haskell-interactive
  :syntax-table haskell-mode-syntax-table

  (setq haskell-interactive-mode-history (list))
  (setq haskell-interactive-mode-history-index 0)

  (setq next-error-function #'haskell-interactive-next-error-function)
  (add-hook 'completion-at-point-functions
            #'haskell-interactive-mode-completion-at-point-function nil t)
  (add-hook 'kill-buffer-hook #'haskell-interactive-kill nil t)
  (haskell-interactive-mode-prompt))

(defvar haskell-interactive-mode-prompt-start
  nil
  "Mark used for the beginning of the prompt.")

(defvar haskell-interactive-mode-result-end
  nil
  "Mark used to figure out where the end of the current result output is.
Used to distinguish betwen user input.")

(defvar-local haskell-interactive-previous-buffer nil
  "Records the buffer to which `haskell-interactive-switch-back' should jump.
This is set by `haskell-interactive-switch', and should otherwise
be nil.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Faces

;;;###autoload
(defface haskell-interactive-face-prompt
  '((t :inherit font-lock-function-name-face))
  "Face for the prompt."
  :group 'haskell-interactive)

;;;###autoload
(defface haskell-interactive-face-prompt2
  '((t :inherit font-lock-keyword-face))
  "Face for the prompt2 in multi-line mode."
  :group 'haskell-interactive)

;;;###autoload
(defface haskell-interactive-face-compile-error
  '((t :inherit compilation-error))
  "Face for compile errors."
  :group 'haskell-interactive)

;;;###autoload
(defface haskell-interactive-face-compile-warning
  '((t :inherit compilation-warning))
  "Face for compiler warnings."
  :group 'haskell-interactive)

;;;###autoload
(defface haskell-interactive-face-result
  '((t :inherit font-lock-string-face))
  "Face for the result."
  :group 'haskell-interactive)

;;;###autoload
(defface haskell-interactive-face-garbage
  '((t :inherit font-lock-string-face))
  "Face for trailing garbage after a command has completed."
  :group 'haskell-interactive)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

(defun haskell-interactive-mode-newline-indent ()
  "Make newline and indent."
  (interactive)
  (newline)
  (indent-to (length haskell-interactive-prompt))
  (indent-relative))

(defun haskell-interactive-mode-kill-whole-line ()
  "Kill the whole REPL line."
  (interactive)
  (kill-region haskell-interactive-mode-prompt-start
               (line-end-position)))

(defun haskell-interactive-switch-back ()
  "Switch back to the buffer from which this interactive buffer was reached."
  (interactive)
  (if haskell-interactive-previous-buffer
      (switch-to-buffer-other-window haskell-interactive-previous-buffer)
    (message "No previous buffer.")))

(defun haskell-interactive-copy-to-prompt ()
  "Copy the current line to the prompt, overwriting the current prompt."
  (interactive)
  (let ((l (buffer-substring-no-properties (line-beginning-position)
                                           (line-end-position))))
    ;; If it looks like the prompt is at the start of the line, chop
    ;; it off.
    (when (and (>= (length l) (length haskell-interactive-prompt))
               (string= (substring l 0 (length haskell-interactive-prompt))
                        haskell-interactive-prompt))
      (setq l (substring l (length haskell-interactive-prompt))))

    (haskell-interactive-mode-set-prompt l)))

(defun haskell-interactive-mode-space (n)
  "Handle the space key."
  (interactive "p")
  (if (and (bound-and-true-p god-local-mode)
           (fboundp 'god-mode-self-insert))
      (call-interactively 'god-mode-self-insert)
    (if (haskell-interactive-at-compile-message)
        (next-error-no-select 0)
      (self-insert-command n))))

(defun haskell-interactive-at-prompt (&optional end-line)
  "If at prompt, return start position of user-input, otherwise return nil.
If END-LINE is non-nil, then return non-nil when the end of line
is at the prompt."
  (if (>= (if end-line (line-end-position) (point))
          haskell-interactive-mode-prompt-start)
      haskell-interactive-mode-prompt-start
    nil))

(define-derived-mode haskell-error-mode
  special-mode "Error"
  "Major mode for viewing Haskell compile errors.")

;; (define-key haskell-error-mode-map (kbd "q") 'quit-window)

(defun haskell-interactive-mode-handle-h ()
  "Handle ^H in output."
  (let ((bound (point-min))
        (inhibit-read-only t))
    (save-excursion
      (while (search-backward "\b" bound t 1)
        (save-excursion
          (forward-char)
          (let ((end (point)))
            (if (search-backward-regexp "[^\b]" bound t 1)
                (forward-char)
              (goto-char (point-min)))
            (let ((start (point)))
              (delete-region (max (- (point) (- end start))
                                  (point-min))
                             end))))))))

(defun haskell-interactive-mode-multi-line (expr)
  "If a multi-line expression EXPR has been entered, then reformat it to be:

:{
do the
   multi-liner
   expr
:}"
  (if (not (string-match-p "\n" expr))
      expr
    (let ((pre (format "^%s" (regexp-quote haskell-interactive-prompt)))
          (lines (split-string expr "\n")))
      (cl-loop for elt on (cdr lines) do
               (setcar elt (replace-regexp-in-string pre "" (car elt))))
      ;; Temporarily set prompt2 to be empty to avoid unwanted output
      (concat ":set prompt2 \"\"\n"
              ":{\n"
              (mapconcat #'identity lines "\n")
              "\n:}\n"
              (format ":set prompt2 \"%s\"" haskell-interactive-prompt2)))))

(defun haskell-interactive-mode-line-is-query (line)
  "Is LINE actually a :t/:k/:i?"
  (and (string-match "^:[itk] " line)
       t))

(defun haskell-interactive-mode-beginning ()
  "Go to the start of the line."
  (interactive)
  (if (haskell-interactive-at-prompt)
      (goto-char haskell-interactive-mode-prompt-start)
    (move-beginning-of-line nil)))

(defun haskell-interactive-mode-input-partial ()
  "Get the interactive mode input up to point."
  (let ((input-start (haskell-interactive-at-prompt)))
    (unless input-start
      (error "not at prompt"))
    (buffer-substring-no-properties input-start (point))))

(defun haskell-interactive-mode-input ()
  "Get the interactive mode input."
  (buffer-substring-no-properties
   haskell-interactive-mode-prompt-start
   (point-max)))

(defun haskell-interactive-mode-prompt (&optional session)
  "Show a prompt at the end of the REPL buffer.
If SESSION is non-nil, use the REPL buffer associated with
SESSION, otherwise operate on the current buffer."
  (with-current-buffer (if session
                           (haskell-session-interactive-buffer session)
                         (current-buffer))
    (save-excursion
      (goto-char (point-max))
      (let ((prompt (propertize haskell-interactive-prompt
                                'font-lock-face 'haskell-interactive-face-prompt
                                'prompt t
                                'read-only haskell-interactive-prompt-read-only
                                'rear-nonsticky t)))
        ;; At the time of writing, front-stickying the first char gives an error
        ;; Has unfortunate side-effect of being able to insert before the prompt
        (insert (substring prompt 0 1)
                (propertize (substring prompt 1)
                            'front-sticky t)))
      (let ((marker (setq-local haskell-interactive-mode-prompt-start (make-marker))))
        (set-marker marker (point))))
    (when (haskell-interactive-at-prompt t)
      (haskell-interactive-mode-scroll-to-bottom))))

(defun haskell-interactive-mode-eval-result (session text)
  "Insert the result of an eval as plain text."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (let ((at-end (eobp))
          (prop-text (propertize text
                                 'font-lock-face 'haskell-interactive-face-result
                                 'front-sticky t
                                 'prompt t
                                 'read-only haskell-interactive-mode-read-only
                                 'rear-nonsticky t
                                 'result t)))
      (save-excursion
        (goto-char (point-max))
        (when (string= text haskell-interactive-prompt2)
          (setq prop-text
                (propertize prop-text
                            'font-lock-face 'haskell-interactive-face-prompt2
                            'read-only haskell-interactive-prompt-read-only)))
        (insert (ansi-color-apply prop-text))
        (haskell-interactive-mode-handle-h)
        (let ((marker (setq-local haskell-interactive-mode-result-end (make-marker))))
          (set-marker marker (point))))
      (when at-end
        (haskell-interactive-mode-scroll-to-bottom)))))

(defun haskell-interactive-mode-scroll-to-bottom ()
  "Scroll to bottom."
  (let ((w (get-buffer-window (current-buffer))))
    (when w
      (goto-char (point-max))
      (set-window-point w (point)))))

(defun haskell-interactive-mode-compile-error (session message)
  "Echo an error."
  (haskell-interactive-mode-compile-message
   session message 'haskell-interactive-face-compile-error))

(defun haskell-interactive-mode-compile-warning (session message)
  "Warning message."
  (haskell-interactive-mode-compile-message
   session message 'haskell-interactive-face-compile-warning))

(defun haskell-interactive-mode-compile-message (session message type)
  "Echo a compiler warning."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (setq next-error-last-buffer (current-buffer))
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (let ((lines (string-match "^\\(.*\\)\n\\([[:unibyte:][:nonascii:]]+\\)" message)))
        (if lines
            (progn
              (insert (propertize (concat (match-string 1 message) " …\n")
                                  'expandable t
                                  'font-lock-face type
                                  'front-sticky t
                                  'read-only haskell-interactive-mode-read-only
                                  'rear-nonsticky t))
              (insert (propertize (concat (match-string 2 message) "\n")
                                  'collapsible t
                                  'font-lock-face type
                                  'front-sticky t
                                  'invisible haskell-interactive-mode-hide-multi-line-errors
                                  'message-length (length (match-string 2 message))
                                  'read-only haskell-interactive-mode-read-only
                                  'rear-nonsticky t)))
          (insert (propertize (concat message "\n")
                              'font-lock-face type
                              'front-sticky t
                              'read-only haskell-interactive-mode-read-only
                              'rear-nonsticky t)))))))

(defun haskell-interactive-mode-insert (session message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (insert (propertize message
                          'front-sticky t
                          'read-only t
                          'rear-nonsticky t)))))

(defun haskell-interactive-mode-goto-end-point ()
  "Go to the 'end' of the buffer (before the prompt)."
  (goto-char haskell-interactive-mode-prompt-start)
  (goto-char (line-beginning-position)))

(defun haskell-interactive-mode-history-add (input)
  "Add INPUT to the history."
  (setq haskell-interactive-mode-history
        (cons ""
              (cons input
                    (cl-remove-if (lambda (i) (or (string= i input) (string= i "")))
                                  haskell-interactive-mode-history))))
  (setq haskell-interactive-mode-history-index
        0))

(defun haskell-interactive-mode-tab ()
  "Do completion if at prompt or else try collapse/expand."
  (interactive)
  (cond
   ((haskell-interactive-at-prompt)
    (completion-at-point))
   ((get-text-property (point) 'collapsible)
    (let ((column (current-column)))
      (search-backward-regexp "^[^ ]")
      (haskell-interactive-mode-tab-expand)
      (goto-char (+ column (line-beginning-position)))))
   (t (haskell-interactive-mode-tab-expand))))

(defun haskell-interactive-mode-tab-expand ()
  "Expand the rest of the message."
  (cond ((get-text-property (point) 'expandable)
         (let* ((pos (1+ (line-end-position)))
                (visibility (get-text-property pos 'invisible))
                (length (1+ (get-text-property pos 'message-length))))
           (let ((inhibit-read-only t))
             (put-text-property pos
                                (+ pos length)
                                'invisible
                                (not visibility)))))))

(defconst haskell-interactive-mode-error-regexp
  "^\\(\\(?:[A-Z]:\\)?[^ \r\n:][^\r\n:]*\\):\\([0-9()-:]+\\):?")

(defun haskell-interactive-at-compile-message ()
  "Am I on a compile message?"
  (and (not (haskell-interactive-at-prompt))
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at haskell-interactive-mode-error-regexp))))

(defun haskell-interactive-mode-error-backward (&optional count)
  "Go backward to the previous error."
  (interactive)
  (search-backward-regexp haskell-interactive-mode-error-regexp nil t count))

(defun haskell-interactive-mode-error-forward (&optional count)
  "Go forward to the next error, or return to the REPL."
  (interactive)
  (goto-char (line-end-position))
  (if (search-forward-regexp haskell-interactive-mode-error-regexp nil t count)
      (progn (goto-char (line-beginning-position))
             t)
    (progn (goto-char (point-max))
           nil)))

(defun haskell-interactive-mode-delete-compile-messages (session &optional file-name)
  "Delete compile messages in REPL buffer.
If FILE-NAME is non-nil, restrict to removing messages concerning
FILE-NAME only."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp "^Compilation failed.$" nil t 1)
        (let ((inhibit-read-only t))
          (delete-region (line-beginning-position)
                         (1+ (line-end-position))))
        (goto-char (point-min)))
      (while (when (re-search-forward haskell-interactive-mode-error-regexp nil t)
               (let ((msg-file-name (match-string-no-properties 1))
                     (msg-startpos (line-beginning-position)))
                 ;; skip over hanging continuation message lines
                 (while (progn (forward-line) (looking-at "^[ ]+")))

                 (when (or (not file-name) (string= file-name msg-file-name))
                   (let ((inhibit-read-only t))
                     (set-text-properties msg-startpos (point) nil))
                   (delete-region msg-startpos (point))
                   ))
               t)))))

;;;###autoload
(defun haskell-interactive-mode-reset-error (session)
  "Reset the error cursor position."
  (interactive)
  (with-current-buffer (haskell-session-interactive-buffer session)
    (haskell-interactive-mode-goto-end-point)
    (let ((mrk (point-marker)))
      (haskell-session-set session 'next-error-locus nil)
      (haskell-session-set session 'next-error-region (cons mrk (copy-marker mrk t))))
    (goto-char (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc

(declare-function haskell-interactive-switch "haskell")
(declare-function haskell-session "haskell")

(defun haskell-session-interactive-buffer (s)
  "Get the session interactive buffer."
  (let ((buffer (haskell-session-get s 'interactive-buffer)))
    (if (and buffer (buffer-live-p buffer))
        buffer
      (let ((buffer-name (format "*%s*" (haskell-session-name s)))
            (index 0))
        (while (get-buffer buffer-name)
          (setq buffer-name (format "*%s <%d>*" (haskell-session-name s) index))
          (setq index (1+ index)))
        (let ((buffer (get-buffer-create buffer-name)))
          (haskell-session-set-interactive-buffer s buffer)
          (with-current-buffer buffer
            (haskell-interactive-mode)
            (haskell-session-assign s))
          (haskell-interactive-switch)
          buffer)))))

(defun haskell-interactive-buffer ()
  "Get the interactive buffer of the session."
  (haskell-session-interactive-buffer (haskell-session)))

(defun haskell-process-cabal-live (state buffer)
  "Do live updates for Cabal processes."
  (haskell-interactive-mode-insert
   (haskell-process-session (cadr state))
   (replace-regexp-in-string
    haskell-process-prompt-regex
    ""
    (substring buffer (cl-cadddr state))))
  (setf (cl-cdddr state) (list (length buffer)))
  nil)

(defun haskell-process-parse-error (string)
  "Parse the line number from the error string STRING."
  (let ((span nil))
    (cl-loop for regex
             in haskell-compilation-error-regexp-alist
             do (when (string-match (car regex) string)
                  (setq span
                        (list :file (match-string 1 string)
                              :line (string-to-number (match-string 2 string))
                              :col (string-to-number (match-string 4 string))
                              :line2 (when (match-string 3 string)
                                       (string-to-number (match-string 3 string)))
                              :col2 (when (match-string 5 string)
                                      (string-to-number (match-string 5 string)))))))
    span))

(defun haskell-process-suggest-add-package (session msg)
  "Add the (matched) module to your cabal file.
Cabal file is selected using SESSION's name, module matching is done in MSG."
  (let* ((suggested-package (match-string 1 msg))
         (package-name (replace-regexp-in-string "-[^-]+$" "" suggested-package))
         (version (progn (string-match "\\([^-]+\\)$" suggested-package)
                         (match-string 1 suggested-package)))
         (cabal-file (concat (haskell-session-name session)
                             ".cabal")))
    (haskell-mode-toggle-interactive-prompt-state)
    (unwind-protect
        (when (y-or-n-p
               (format "Add `%s' to %s?"
                       package-name
                       cabal-file))
          (haskell-cabal-add-dependency package-name version nil t)
          (when (y-or-n-p (format "Enable -package %s in the GHCi session?" package-name))
            (haskell-process-queue-without-filters
             (haskell-session-process session)
             (format ":set -package %s" package-name))))
      (haskell-mode-toggle-interactive-prompt-state t))))

(defun haskell-process-suggest-remove-import (session file import line)
  "Suggest removing or commenting out import statement.
Asks user to handle redundant import statement using interactive
SESSION in specified FILE to remove IMPORT on given LINE."
  (let ((first t))
    (haskell-mode-toggle-interactive-prompt-state)
    (unwind-protect
        (cl-case (read-event
                  (propertize (format "%sThe import line `%s' is redundant. Remove? (y, n, c: comment out)  "
                                      (if (not first)
                                          "Please answer n, y or c: "
                                        "")
                                      import)
                              'face
                              'minibuffer-prompt))
          (?y
           (haskell-process-find-file session file)
           (save-excursion
             (goto-char (point-min))
             (forward-line (1- line))
             (goto-char (line-beginning-position))
             (delete-region (line-beginning-position)
                            (line-end-position))))
          (?n
           (message "Ignoring redundant import %s" import))
          (?c
           (haskell-process-find-file session file)
           (save-excursion
             (goto-char (point-min))
             (forward-line (1- line))
             (goto-char (line-beginning-position))
             (insert "-- "))))
      ;; unwind
      (haskell-mode-toggle-interactive-prompt-state t))))

(defun haskell-process-find-file (session file)
  "Find the given file in the project."
  (find-file (cond ((file-exists-p (concat (haskell-session-current-dir session) "/" file))
                    (concat (haskell-session-current-dir session) "/" file))
                   ((file-exists-p (concat (haskell-session-cabal-dir session) "/" file))
                    (concat (haskell-session-cabal-dir session) "/" file))
                   (t file))))

(defun haskell-process-suggest-pragma (session pragma extension file)
  "Suggest to add something to the top of the file.
SESSION is used to search given file.  Adds PRAGMA and EXTENSION
wrapped in compiler directive at the top of FILE."
  (let ((string  (format "{-# %s %s #-}" pragma extension)))
    (haskell-mode-toggle-interactive-prompt-state)
    (unwind-protect
        (when (y-or-n-p (format "Add %s to the top of the file? " string))
          (haskell-process-find-file session file)
          (save-excursion
            (goto-char (point-min))
            (insert (concat string "\n"))))
      (haskell-mode-toggle-interactive-prompt-state t))))

(defun haskell-interactive-mode-insert-error (response)
  "Insert an error message."
  (insert "\n"
          (haskell-fontify-as-mode
           response
           'haskell-mode))
  (haskell-interactive-mode-prompt))

(defun haskell-interactive-popup-error (response)
  "Popup an error."
  (if haskell-interactive-popup-errors
      (let ((buf (get-buffer-create "*HS-Error*")))
        (pop-to-buffer buf nil t)
        (with-current-buffer buf

          (haskell-error-mode)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize response
                                'font-lock-face
                                'haskell-interactive-face-compile-error))
            (goto-char (point-min))
            (delete-blank-lines)
            (insert (propertize "-- Hit `q' to close this window.\n\n"
                                'font-lock-face 'font-lock-comment-face))
            (save-excursion
              (goto-char (point-max))
              (insert (propertize "\n-- To disable popups, customize `haskell-interactive-popup-errors'.\n\n"
                                  'font-lock-face 'font-lock-comment-face))))))
    (haskell-interactive-mode-insert-error response)))

(defun haskell-interactive-next-error-function (&optional n reset)
  "See `next-error-function' for more information."

  (let* ((session (haskell-interactive-session))
         (next-error-region (haskell-session-get session 'next-error-region))
         (next-error-locus (haskell-session-get session 'next-error-locus))
         (reset-locus nil))

    (when (and next-error-region (or reset (and (/= n 0) (not next-error-locus))))
      (goto-char (car next-error-region))
      (unless (looking-at haskell-interactive-mode-error-regexp)
        (haskell-interactive-mode-error-forward))

      (setq reset-locus t)
      (unless (looking-at haskell-interactive-mode-error-regexp)
        (error "no errors found")))

    ;; move point if needed
    (cond
     (reset-locus nil)
     ((> n 0) (unless (haskell-interactive-mode-error-forward n)
                (error "no more errors")))

     ((< n 0) (unless (haskell-interactive-mode-error-backward (- n))
                (error "no more errors"))))

    (let ((orig-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

      (when (string-match haskell-interactive-mode-error-regexp orig-line)
        (let* ((msgmrk (set-marker (make-marker) (line-beginning-position)))
               (location (haskell-process-parse-error orig-line))
               (file (plist-get location :file))
               (line (plist-get location :line))
               (col1 (plist-get location :col))
               (col2 (plist-get location :col2))

               (cabal-relative-file (expand-file-name file (haskell-session-cabal-dir session)))
               (src-relative-file (expand-file-name file (haskell-session-current-dir session)))

               (real-file (cond ((file-exists-p cabal-relative-file) cabal-relative-file)
                                ((file-exists-p src-relative-file) src-relative-file))))

          (haskell-session-set session 'next-error-locus msgmrk)

          (if real-file
              (let ((m1 (make-marker))
                    (m2 (make-marker)))
                (with-current-buffer (find-file-noselect real-file)
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- line))
                    (set-marker m1 (+ col1 (point) -1))

                    (when col2
                      (set-marker m2 (- (point) col2)))))
                ;; ...finally select&hilight error locus
                (compilation-goto-locus msgmrk m1 (and (marker-position m2) m2)))
            (error "don't know where to find %S" file)))))))

(defun haskell-interactive-session ()
  "Get the `haskell-session', throw an error if it's not available."
  (or (haskell-session-maybe)
      (haskell-session-assign
       (or (haskell-session-from-buffer)
           (haskell-session-choose)
           (error "No session associated with this buffer. Try M-x haskell-session-change or report this as a bug.")))))

(defun haskell-interactive-process ()
  "Get the Haskell session."
  (or (haskell-session-process (haskell-interactive-session))
      (error "No Haskell session/process associated with this
      buffer. Maybe run M-x haskell-process-restart?")))

(defun haskell-interactive-mode-do-presentation (expr)
  "Present the given expression EXPR.
Requires the `present' package to be installed.
Will automatically import it qualified as Present."
  (let ((p (haskell-interactive-process)))
    ;; If Present.code isn't available, we probably need to run the
    ;; setup.
    (unless (string-match "^Present" (haskell-process-queue-sync-request p ":t Present.encode"))
      (haskell-interactive-mode-setup-presentation p))
    ;; Happily, let statements don't affect the `it' binding in any
    ;; way, so we can fake it, no pun intended.
    (let ((error (haskell-process-queue-sync-request
                  p (concat "let it = Present.asData (" expr ")"))))
      (if (not (string= "" error))
          (haskell-interactive-mode-eval-result (haskell-interactive-session) (concat error "\n"))
        (let ((hash (haskell-interactive-mode-presentation-hash)))
          (haskell-process-queue-sync-request
           p (format "let %s = Present.asData (%s)" hash expr))
          (let* ((presentation (haskell-interactive-mode-present-id
                                hash
                                (list 0))))
            (insert "\n")
            (haskell-interactive-mode-insert-presentation hash presentation)
            (haskell-interactive-mode-eval-result (haskell-interactive-session) "\n"))))
      (haskell-interactive-mode-prompt (haskell-interactive-session)))))

(defun haskell-interactive-mode-present-id (hash id)
  "Generate a presentation for the current expression at ID."
  ;; See below for commentary of this statement.
  (let ((p (haskell-interactive-process)))
    (haskell-process-queue-without-filters
     p "let _it = it")
    (let* ((text (haskell-process-queue-sync-request
                  p
                  (format "Present.putStr (Present.encode (Present.fromJust (Present.present (Present.fromJust (Present.fromList [%s])) %s)))"
                          (mapconcat 'identity (mapcar 'number-to-string id) ",")
                          hash)))
           (reply
            (if (string-match "^*** " text)
                '((rep nil))
              (read text))))
      ;; Not necessary, but nice to restore it to the expression that
      ;; the user actually typed in.
      (haskell-process-queue-without-filters
       p "let it = _it")
      reply)))

(defun haskell-presentation-present-slot (btn)
  "The callback to evaluate the slot and present it in place of the button BTN."
  (let ((id (button-get btn 'presentation-id))
        (hash (button-get btn 'hash))
        (parent-rep (button-get btn 'parent-rep))
        (continuation (button-get btn 'continuation)))
    (let ((point (point)))
      (button-put btn 'invisible t)
      (delete-region (button-start btn) (button-end btn))
      (haskell-interactive-mode-insert-presentation
       hash
       (haskell-interactive-mode-present-id hash id)
       parent-rep
       continuation)
      (when (> (point) point)
        (goto-char (1+ point))))))

(defun haskell-interactive-mode-presentation-slot (hash slot parent-rep &optional continuation)
  "Make a slot at point, pointing to ID."
  (let ((type (car slot))
        (id (cadr slot)))
    (if (member (intern type) '(Integer Char Int Float Double))
        (haskell-interactive-mode-insert-presentation
         hash
         (haskell-interactive-mode-present-id hash id)
         parent-rep
         continuation)
      (haskell-interactive-mode-presentation-slot-button slot parent-rep continuation hash))))

(defun haskell-interactive-mode-presentation-slot-button (slot parent-rep continuation hash)
  (let ((start (point))
        (type (car slot))
        (id (cadr slot)))
    (insert (propertize type 'font-lock-face '(:height 0.8 :underline t :inherit font-lock-comment-face)))
    (let ((button (make-text-button start (point)
                                    :type 'haskell-presentation-slot-button)))
      (button-put button 'hide-on-click t)
      (button-put button 'presentation-id id)
      (button-put button 'parent-rep parent-rep)
      (button-put button 'continuation continuation)
      (button-put button 'hash hash))))

(defun haskell-interactive-mode-insert-presentation (hash presentation &optional parent-rep continuation)
  "Insert the presentation, hooking up buttons for each slot."
  (let* ((rep (cadr (assoc 'rep presentation)))
         (text (cadr (assoc 'text presentation)))
         (slots (cadr (assoc 'slots presentation)))
         (nullary (null slots)))
    (cond
     ((string= "integer" rep)
      (insert (propertize text 'font-lock-face 'font-lock-constant)))
     ((string= "floating" rep)
      (insert (propertize text 'font-lock-face 'font-lock-constant)))
     ((string= "char" rep)
      (insert (propertize
               (if (string= "string" parent-rep)
                   (replace-regexp-in-string "^'\\(.+\\)'$" "\\1" text)
                 text)
               'font-lock-face 'font-lock-string-face)))
     ((string= "tuple" rep)
      (insert "(")
      (let ((first t))
        (cl-loop for slot in slots
                 do (unless first (insert ","))
                 do (haskell-interactive-mode-presentation-slot hash slot rep)
                 do (setq first nil)))
      (insert ")"))
     ((string= "list" rep)
      (if (null slots)
          (if continuation
              (progn (delete-char -1)
                     (delete-indentation))
            (insert "[]"))
        (let ((i 0))
          (unless continuation
            (insert "["))
          (let ((start-column (current-column)))
            (cl-loop for slot in slots
                     do (haskell-interactive-mode-presentation-slot
                         hash
                         slot
                         rep
                         (= i (1- (length slots))))
                     do (when (not (= i (1- (length slots))))
                          (insert "\n")
                          (indent-to (1- start-column))
                          (insert ","))
                     do (setq i (1+ i))))
          (unless continuation
            (insert "]")))))
     ((string= "string" rep)
      (unless (string= "string" parent-rep)
        (insert (propertize "\"" 'font-lock-face 'font-lock-string-face)))
      (cl-loop for slot in slots
               do (haskell-interactive-mode-presentation-slot hash slot rep))
      (unless (string= "string" parent-rep)
        (insert (propertize "\"" 'font-lock-face 'font-lock-string-face))))
     ((string= "alg" rep)
      (when (and parent-rep
                 (not nullary)
                 (not (string= "list" parent-rep)))
        (insert "("))
      (let ((start-column (current-column)))
        (insert (propertize text 'font-lock-face 'font-lock-type-face))
        (cl-loop for slot in slots
                 do (insert "\n")
                 do (indent-to (+ 2 start-column))
                 do (haskell-interactive-mode-presentation-slot hash slot rep)))
      (when (and parent-rep
                 (not nullary)
                 (not (string= "list" parent-rep)))
        (insert ")")))
     ((string= "record" rep)
      (let ((start-column (current-column)))
        (insert (propertize text 'font-lock-face 'font-lock-type-face)
                " { ")
        (cl-loop for field in slots
                 do (insert "\n")
                 do (indent-to (+ 2 start-column))
                 do (let ((name (nth 0 field))
                          (slot (nth 1 field)))
                      (insert name " = ")
                      (haskell-interactive-mode-presentation-slot hash slot rep)))
        (insert "\n")
        (indent-to start-column)
        (insert "}")))
     ((eq rep nil)
      (insert (propertize "?" 'font-lock-face 'font-lock-warning)))
     (t
      (let ((err "Unable to present! This very likely means Emacs
is out of sync with the `present' package. You should make sure
they're both up to date, or report a bug."))
        (insert err)
        (error err))))))

(defun haskell-interactive-mode-setup-presentation (p)
  "Setup the GHCi REPL for using presentations.

Using asynchronous queued commands as opposed to sync at this
stage, as sync would freeze up the UI a bit, and we actually
don't care when the thing completes as long as it's soonish."
  ;; Import dependencies under Present.* namespace
  (haskell-process-queue-without-filters p "import qualified Data.Maybe as Present")
  (haskell-process-queue-without-filters p "import qualified Data.ByteString.Lazy as Present")
  (haskell-process-queue-without-filters p "import qualified Data.AttoLisp as Present")
  (haskell-process-queue-without-filters p "import qualified Present.ID as Present")
  (haskell-process-queue-without-filters p "import qualified Present as Present")
  ;; Make a dummy expression to avoid "Loading package" nonsense
  (haskell-process-queue-without-filters
   p "Present.present (Present.fromJust (Present.fromList [0])) ()"))

(defvar haskell-interactive-mode-presentation-hash 0
  "Counter for the hash.")

(defun haskell-interactive-mode-presentation-hash ()
  "Generate a presentation hash."
  (format "_present_%s"
          (setq haskell-interactive-mode-presentation-hash
                (1+ haskell-interactive-mode-presentation-hash))))

(define-button-type 'haskell-presentation-slot-button
  'action 'haskell-presentation-present-slot
  'follow-link t
  'help-echo "Click to expand…")

(defun haskell-interactive-mode-history-toggle (n)
  "Toggle the history N items up or down."
  (unless (null haskell-interactive-mode-history)
    (setq haskell-interactive-mode-history-index
          (mod (+ haskell-interactive-mode-history-index n)
               (length haskell-interactive-mode-history)))
    (unless (zerop haskell-interactive-mode-history-index)
      (message "History item: %d" haskell-interactive-mode-history-index))
    (haskell-interactive-mode-set-prompt
     (nth haskell-interactive-mode-history-index
          haskell-interactive-mode-history))))

(defun haskell-interactive-mode-set-prompt (p)
  "Set (and overwrite) the current prompt."
  (with-current-buffer (haskell-session-interactive-buffer (haskell-interactive-session))
    (goto-char haskell-interactive-mode-prompt-start)
    (delete-region (point) (point-max))
    (insert p)))

(defun haskell-interactive-mode-history-previous (arg)
  "Cycle backwards through input history."
  (interactive "*p")
  (when (haskell-interactive-at-prompt)
    (if (not (zerop arg))
        (haskell-interactive-mode-history-toggle arg)
      (setq haskell-interactive-mode-history-index 0)
      (haskell-interactive-mode-history-toggle 1))))

(defun haskell-interactive-mode-history-next (arg)
  "Cycle forward through input history."
  (interactive "*p")
  (when (haskell-interactive-at-prompt)
    (if (not (zerop arg))
        (haskell-interactive-mode-history-toggle (- arg))
      (setq haskell-interactive-mode-history-index 0)
      (haskell-interactive-mode-history-toggle -1))))

(defun haskell-interactive-mode-prompt-previous ()
  "Jump to the previous prompt."
  (interactive)
  (let ((prev-prompt-pos
         (save-excursion
           (beginning-of-line) ;; otherwise prompt at current line matches
           (and (search-backward-regexp (haskell-interactive-prompt-regex) nil t)
                (match-end 0)))))
    (when prev-prompt-pos (goto-char prev-prompt-pos))))

(defun haskell-interactive-mode-prompt-next ()
  "Jump to the next prompt."
  (interactive)
  (search-forward-regexp (haskell-interactive-prompt-regex) nil t))

(defun haskell-interactive-mode-clear ()
  "Clear the screen and put any current input into the history."
  (interactive)
  (let ((session (haskell-interactive-session)))
    (with-current-buffer (haskell-session-interactive-buffer session)
      (let ((inhibit-read-only t))
        (set-text-properties (point-min) (point-max) nil))
      (delete-region (point-min) (point-max))
      (remove-overlays)
      (haskell-interactive-mode-prompt session)
      (haskell-session-set session 'next-error-region nil)
      (haskell-session-set session 'next-error-locus nil))
    (with-current-buffer (get-buffer-create "*haskell-process-log*")
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max)))
      (remove-overlays))))

(defun haskell-interactive-mode-completion-at-point-function ()
  "Offer completions for partial expression between prompt and point.
This completion function is used in interactive REPL buffer itself."
  (when (haskell-interactive-at-prompt)
    (let* ((process (haskell-interactive-process))
           (inp (haskell-interactive-mode-input-partial))
           (resp2 (haskell-process-get-repl-completions process inp))
           (rlen (-  (length inp) (length (car resp2))))
           (coll (append (if (string-prefix-p inp "import") '("import"))
                         (if (string-prefix-p inp "let") '("let"))
                         (cdr resp2))))
      (list (- (point) rlen) (point) coll))))

(defun haskell-interactive-mode-trigger-compile-error (state response)
  "Look for an <interactive> compile error.
If there is one, pop that up in a buffer, similar to `debug-on-error'."
  (when (and haskell-interactive-types-for-show-ambiguous
             (string-match "^\n<interactive>:[-0-9]+:[-0-9]+:" response)
             (not (string-match "^\n<interactive>:[-0-9]+:[-0-9]+:[\n ]+[Ww]arning:" response)))
    (let ((inhibit-read-only t))
      (delete-region haskell-interactive-mode-prompt-start (point))
      (set-marker haskell-interactive-mode-prompt-start
                  haskell-interactive-mode-old-prompt-start)
      (goto-char (point-max)))
    (cond
     ((and (not (haskell-interactive-mode-line-is-query (elt state 2)))
           (or (string-match "No instance for (?Show[ \n]" response)
               (string-match "Ambiguous type variable " response)))
      (haskell-process-reset (haskell-interactive-process))
      (let ((resp (haskell-process-queue-sync-request
                   (haskell-interactive-process)
                   (concat ":t "
                           (buffer-substring-no-properties
                            haskell-interactive-mode-prompt-start
                            (point-max))))))
        (cond
         ((not (string-match "<interactive>:" resp))
          (haskell-interactive-mode-insert-error resp))
         (t (haskell-interactive-popup-error response)))))
     (t (haskell-interactive-popup-error response)
        t))
    t))

;;;###autoload
(defun haskell-interactive-mode-echo (session message &optional mode)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (insert (if mode
                  (haskell-fontify-as-mode
                   (concat message "\n")
                   mode)
                (propertize (concat message "\n")
                            'front-sticky t
                            'read-only t
                            'rear-nonsticky t))))))

(defun haskell-interactive-mode-splices-buffer (session)
  "Get the splices buffer for the current SESSION."
  (get-buffer-create (haskell-interactive-mode-splices-buffer-name session)))

(defun haskell-interactive-mode-splices-buffer-name (session)
  (format "*%s:splices*" (haskell-session-name session)))

(defun haskell-interactive-mode-compile-splice (session message)
  "Echo a compiler splice."
  (with-current-buffer (haskell-interactive-mode-splices-buffer session)
    (unless (eq major-mode 'haskell-mode)
      (haskell-mode))
    (let* ((parts (split-string message "\n  ======>\n"))
           (file-and-decl-lines (split-string (nth 0 parts) "\n"))
           (file (nth 0 file-and-decl-lines))
           (decl (mapconcat #'identity (cdr file-and-decl-lines) "\n"))
           (output (nth 1 parts)))
      (insert "-- " file "\n")
      (let ((start (point)))
        (insert decl "\n")
        (indent-rigidly start (point) -4))
      (insert "-- =>\n")
      (let ((start (point)))
        (insert output "\n")
        (indent-rigidly start (point) -4)))))

(defun haskell-interactive-mode-insert-garbage (session message)
  "Echo a read only piece of text before the prompt."
  (with-current-buffer (haskell-session-interactive-buffer session)
    (save-excursion
      (haskell-interactive-mode-goto-end-point)
      (insert (propertize message
                          'front-sticky t
                          'font-lock-face 'haskell-interactive-face-garbage
                          'read-only t
                          'rear-nonsticky t)))))

;;;###autoload
(defun haskell-process-show-repl-response (line)
  "Send LINE to the GHCi process and echo the result in some fashion.
Result will be printed in the minibuffer or presented using
function `haskell-presentation-present', depending on variable
`haskell-process-use-presentation-mode'."
  (let ((process (haskell-interactive-process)))
    (haskell-process-queue-command
     process
     (make-haskell-command
      :state (cons process line)
      :go (lambda (state)
            (haskell-process-send-string (car state) (cdr state)))
      :complete (lambda (state response)
                  (if haskell-process-use-presentation-mode
                      (haskell-presentation-present
                       (haskell-process-session (car state))
                       response)
                    (haskell-mode-message-line response)))))))

(provide 'haskell-interactive-mode)

;;; haskell-interactive-mode.el ends here
