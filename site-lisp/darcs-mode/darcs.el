;;; darcs.el -- version control commands for Darcs.
;;; Copyright 2004  Luke Gorrie <luke@member.fsf.org>
;;;
;;; darcs.el is free software distributed under the terms of the GNU
;;; General Public Licence, version 2. For details see the file
;;; COPYING in the GNU Emacs distribution.
;;;
;;; This is a minor-mode for Darcs-controlled files. The mode includes
;;; just a few hard-to-live-without commands and is automatically
;;; activated when a file from a Darcs tree is opened. Note that
;;; pressing `q' should neatly dismiss any buffer that darcs.el pops
;;; up, view-mode quirks aside.
;;;
;;; In a mixture of frugality and indulgence I avoided the Common Lisp
;;; compatibility library but in small ways target GNU Emacs 21 on
;;; Unix.

(require 'easy-mmode)
(require 'outline)

;;;; Configurables

;; I set this to "\C-cd" in my ~/.emacs. -luke (10/Jul/2004)
(defvar darcs-command-prefix "\C-c\C-v"
  "*Prefix key sequence for darcs-mode commands.
You should set this variable before loading this file.")

(defvar darcs-buffer "*darcs-command*"
  "Name of the buffer used to present output from Darcs commands.")

;;;; Minor-mode

(define-minor-mode darcs-mode
  "\\{darcs-mode-map}"
  nil
  " Darcs"
  ;; Coax define-minor-mode into creating a keymap.
  ;; We'll fill it in manually though because define-minor-mode seems
  ;; hopeless for changing bindings without restarting Emacs.
  `((,darcs-command-prefix . fake)))

(defvar darcs-mode-commands-map nil
  "Keymap for darcs-mode commands.
This map is bound to a prefix sequence in `darcs-mode-map'.")

(defconst darcs-command-keys '(("d" darcs-diff)
                               ("e" darcs-ediff)
                               ("/" darcs-revert)
                               ("w" darcs-whatsnew)
                               ("s" darcs-whatsnew-summary)
                               ("c" darcs-changes)
                               ("l" darcs-changes/outline))
  "Keys to bind in `darcs-mode-commands-map'.")

(defun darcs-init-command-keymap ()
  "Bind the darcs-mode keys.
This command can be called interactively to redefine the keys from
`darcs-commands-keys'."
  (interactive)
  (setq darcs-mode-commands-map (make-sparse-keymap))
  (dolist (spec darcs-command-keys)
    (define-key darcs-mode-commands-map (car spec) (cadr spec)))
  (define-key darcs-mode-map darcs-command-prefix darcs-mode-commands-map))

(darcs-init-command-keymap)

;;;; Commands

(defun darcs-diff (filename)
  "Diff the current buffer's file against the repo version.
With a prefix argument prompt for the file to diff."
  (interactive (list (darcs-current-file)))
  (darcs-maybe-save filename)
  (darcs-run-command (darcs-file-command "darcs diff %s" filename) 'diff-mode))

(unless (boundp 'temporary-file-directory)
    (defvar temporary-file-directory
      (file-name-as-directory (temp-directory))))

(defun darcs-ediff ()
  "Ediff the current buffer against the repo version."
  ;; I couldn't find any Darcs command to pull out the current repo
  ;; version of a file, so I just copy it directly from the repo dir.
  ;; -luke (10/Jul/2004)
  (interactive)
  ;; This function does a lot of filename munging. The comments are
  ;; based on the example that the file we want to diff is
  ;; /foo/bar/baz.c and its darcs directory is /foo/_darcs/
  (darcs-maybe-save (darcs-current-file))
  (let* ((dir (darcs-directory))
         (target-path (darcs-current-file)))
    ;; dir = /foo/_darcs/
    ;; target-path = /foo/bar/baz.c
    (unless dir
      (error "Can't find _darcs directory!"))
    (unless target-path
      (error "Current buffer is not associated with a file!"))
    (let ((stem (file-name-directory dir)))
      ;; stem = /foo/
      (string-match (concat "^" (regexp-quote stem) "\\(.*\\)$")
                    target-path)
      (let* ((uniq (match-string 1 target-path))
             (repo (concat stem "_darcs/current/" uniq))
             (target-name (file-name-nondirectory target-path))
             (temp (concat temporary-file-directory "REPO~" target-name)))
        ;; uniq = bar/baz.c
        ;; repo = /foo/_darcs/current/bar/baz.c
        ;; temp = /tmp/REPO~baz.c  (ugly name to stand out in the modeline)
        (when (get-file-buffer temp)
          (kill-buffer (get-file-buffer temp)))
        (copy-file repo temp t)
        (with-current-buffer (find-file-noselect temp)
          (setq buffer-read-only t)
          (view-buffer-other-window (current-buffer)))
        (ediff-files temp target-path)))))

(defun darcs-whatsnew ()
  "Show 'whatsnew' in a message or buffer."
  (interactive)
  (darcs-run-command "darcs whatsnew --no-summary" 'diff-mode))

(defun darcs-whatsnew-summary ()
  "Show 'whatsnew -s' in a message or buffer."
  (interactive)
  (darcs-run-command "darcs whatsnew --summary"))

(defun darcs-revert (filename)
  "Revert unrecorded changes to the current file."
  (interactive (list (darcs-current-file)))
  (darcs-maybe-save filename)
  (save-window-excursion
    (darcs-diff (darcs-current-file))
    (if (not (y-or-n-p "Revert changes? "))
        (message "Cancelled.")
      (save-window-excursion
        (darcs-run-command (darcs-file-command "echo y | darcs revert --all %s"
                                               filename)))
      (darcs-revert-file filename)
      (message "Reverted."))))

(defun darcs-changes (filename)
  "Show 'darcs changes' for a file or directory."
  (interactive (list (darcs-read-file-name)))
  (darcs-run-command (darcs-file-command "darcs changes %s" filename)))

(defun darcs-changes/outline (filename)
  "Show tweaked `darcs-changes' in an outline-mode buffer."
  (interactive (list (darcs-read-file-name)))
  (darcs-run-command (darcs-file-command "darcs changes %s" filename)
                     'outline-mode)
  (with-current-buffer darcs-buffer
    ;; We tweak the output a little bit to make it
    ;; outline-mode-friendly. The headings are patch names.
    ;;
    ;; Each change normally starts with a pair of lines like:
    ;;
    ;; Sun Jul 11 08:09:09 CEST 2004  Luke Gorrie <luke@member.fsf.org>
    ;;   * doc, programmed exports, checksums as errors
    ;;
    ;; We change this to:
    ;; 
    ;; * doc, programmed exports, checksums as errors
    ;;   Sun Jul 11 08:09:09 CEST 2004  Luke Gorrie <luke@member.fsf.org>
    ;;
    ;; We also ensure that the lines before headings aren't blank
    ;; (that they contain whitespace) so that outline-mode will fold
    ;; them.
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      ;; Get rid of the opening 'Changes to:' line if it's there.
      (when (looking-at "Changes to ")
        (save-excursion
          (forward-line)
          (when (looking-at "^$") (forward-line))
          (delete-region (point-min) (point))))
      (goto-char (point-max))
      (while (re-search-backward "^[A-Z]" nil t)
        (save-excursion
          ;; Previous line should not be blank.
          (save-excursion (forward-line -1)
                          (when (looking-at "^$") (insert " ")))
          ;; Transpose patch name and date/author lines.
          (forward-line)
          (save-excursion (transpose-lines 1))
          ;; Hack patch name.
          (delete-horizontal-space)
          ;; Hack date/author.
          (forward-line)
          (insert "  ")
          (darcs-face-change-date-line))))
    (set (make-local-variable 'outline-regexp) "\\*")
    (hide-body)))

(defun darcs-face-change-date-line ()
  "Add some pretty faces to a date-and-who change line."
  ;; We separately face these bits:
  ;;   Sun Jul 11 08:09:09 CEST 2004  Luke Gorrie <luke@member.fsf.org>
  ;;   ^^ date ^^^^^^^^^^^^^^^^^^^^^  ^^ name ^^^  ^^ email ^^^^^^^^^^
  ;; borrowing the faces from change-log-mode for convenience.
  ;;
  ;; Since this is generally lossy output-text-munging we shouldn't
  ;; trigger an error if the line doesn't look like we expect.
  (save-excursion
    (let ((end (line-end-position)))
      (when (re-search-forward ".*  " end t)
        (darcs-set-match-face 'change-log-date-face)
        (when (re-search-forward "[^<]+" end t)
          (darcs-set-match-face 'change-log-name-face)
          (when (re-search-forward "[^<][^>]+" end t)
            (darcs-set-match-face 'change-log-email-face)))))))

(defun darcs-set-match-face (face &optional match)
  "Apply FACE to the region of MATCH.
The face is set with an overlay to avoid font-lock interaction."
  (let* ((match (or match 0))
         (start (match-beginning match))
         (end   (match-end match))
         (overlay (make-overlay start end)))
    (overlay-put overlay 'face face)))

;;;; Utilities

(defun darcs-run-command (command &optional pre-view-hook)
  "Run COMMAND and view the result in another window.
PRE-VIEW-HOOK is an optional function to call before entering
view-mode. This is useful to set the major-mode of the result buffer,
because if you did it afterwards then it would zap view-mode."
  (darcs-cleanup)
  (with-current-buffer (get-buffer-create darcs-buffer)
    ;; prevent `shell-command' from printing output in a message
    (let ((max-mini-window-height 0))
      (shell-command command t))
    (goto-char (point-min))
    (when pre-view-hook
      (funcall pre-view-hook)))
  (view-buffer-other-window darcs-buffer))

(defun darcs-file-command (command-string filename)
  "Return a shell command string based on COMMAND-STRING.
The new command string will start be changing into the directory of
FILENAME. If COMMAND-STRING contains a \"%s\" format directive then it
will be substituted with the filename (sans directory)."
  (let* ((name (file-name-nondirectory filename))
         ;; Kludgy..
         (new-cmd (condition-case nil
                      (format command-string name)
                    (error command-string))))
    (if (file-name-directory filename)
        (format "cd \"%s\"; %s"
                (expand-file-name (file-name-directory filename))
                new-cmd)
      new-cmd)))

(defun darcs-current-file ()
  (or (buffer-file-name)
      (error "Don't know what file to use!")))

(defun darcs-maybe-save (filename)
  "Offer to FILENAME if modified."
  (let ((buffer (get-file-buffer filename)))
    (when (and buffer
               (buffer-modified-p buffer)
               (y-or-n-p (format "Save file %s? " filename)))
      (save-buffer buffer))))

(defun darcs-revert-file (filename)
  "Reload FILENAME from disk."
  (when (get-file-buffer filename)
    (with-current-buffer (get-file-buffer filename)
      (revert-buffer t t t))))

(defun darcs-directory (&optional start-directory)
  "Return the enclosing \"_darcs\" directory, or nil if there isn't one."
  (let ((dir (or start-directory
                 default-directory
                 (error "No start directory given."))))
    (or (car (directory-files dir t "^_darcs$"))
        (let ((next-dir (file-name-directory (directory-file-name dir))))
          (unless (equal dir next-dir)
            (darcs-directory next-dir))))))

(defun darcs-cleanup (&optional buffer-name)
  "Cleanup before executing a command.
BUFFER-NAME is the command's output buffer."
  (let ((name (or buffer-name darcs-buffer)))
    (when (get-buffer darcs-buffer)
      (kill-buffer darcs-buffer))))

(defun darcs-read-file-name ()
  "Read the name of a file or directory, hinting at the current file."
  (read-file-name "File or directory: " nil nil t
                  (when (buffer-file-name)
                    (file-name-nondirectory (buffer-file-name)))))

;;;; Test case
;;;
;;; This test function is called by Darcs itself as a repository
;;; invariant.

(defun darcs-repo-test ()
  "Check that darcs.el byte-compiles without warnings.
Exit Emacs with non-zero status on failure.

This is a repository-invariant function for the repo's test."
  (let ((byte-compile-error-on-warn t))
    (unless (byte-compile-file "darcs.el")
      (kill-emacs 1))))

;;;; Hook setup
;;;
;;; Automatically enter darcs-mode when we open a file that's under
;;; Darcs control.

(defun darcs-find-file-hook ()
  "Enable darcs-mode if the current file is under Darcs control."
  (when (darcs-directory) (darcs-mode 1)))

(add-hook 'find-file-hooks 'darcs-find-file-hook)

(provide 'darcs)
