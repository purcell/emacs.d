;;; sbt-mode.el --- Interactive support for sbt projects

;; Copyright (c) 2013 Heikki Vesalainen

;; Homepage: https://github.com/ensime/emacs-sbt-mode
;; Keywords: languages
;; Package-Version:  0.2
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:
;;
;;  Documentation at http://ensime.github.io/editors/emacs/sbt-mode/
;;
;;; Code:

(require 'compile)
(require 'comint)
(require 'sbt-mode-vars)
(require 'sbt-mode-project)
(require 'sbt-mode-buffer)
(require 'sbt-mode-comint)
(require 'sbt-mode-rgrep)

(eval-when-compile
  (defvar sbt:submode)
  (defun scala-mode:set-scala-syntax-mode ()))

(defvar sbt:error-face 'sbt:error)
(defvar sbt:info-face 'sbt:info)
(defvar sbt:warning-face 'sbt:warning)

(defvar-local sbt:previous-command sbt:default-command)

(defvar sbt:command-history-temp nil)

;;;
;;; Our user commands
;;;

;;;###autoload
(defun sbt-start () "Start sbt" (interactive) (sbt:run-sbt nil t))

(defun sbt-clear ()
  "Clear the current sbt buffer and send RET to sbt to re-display the prompt"
  (interactive) (sbt:clear))

;;;###autoload
(defun run-scala ()
  "Pop up Scala REPL buffer.

If the sbt buffer is not in REPL mode, it will switch to REPL mode (console)."
  (interactive)
  (if (not (comint-check-proc (sbt:buffer-name)))
      (sbt-command "console")
    (with-current-buffer (sbt:buffer-name)
      (when (eq sbt:submode 'sbt)
        (sbt-command "console")))
    (pop-to-buffer (sbt:buffer-name))))

;;;###autoload
(defun sbt-command (command &optional focus)
  "Send a command to the sbt process of the current buffer's sbt project.
Prompts for the command to send when in interactive mode. You can
use tab completion.

This command does the following:
  - displays the buffer moving focus to it if focus is t
  - erases the buffer
  - forgets about compilation errors

The command is most usefull for running a compilation command
that outputs errors."
  (interactive
   (progn
     (setq sbt:command-history-temp
           (ignore-errors (with-current-buffer (sbt:buffer-name) (ring-elements comint-input-ring))))

     (list (completing-read (format "Command to run (default %s): " (sbt:get-previous-command))
                            (completion-table-dynamic 'sbt:get-sbt-completions-for-command)
                            nil nil nil 'sbt:command-history-temp (sbt:get-previous-command)))))
  (sbt:command command focus)
  (with-current-buffer (sbt:buffer-name)
    (setq sbt:previous-command command)))

(defun sbt:get-sbt-completions-for-command (input)
  (ignore-errors (with-current-buffer (sbt:buffer-name) (sbt:get-sbt-completions input))))

;;;###autoload
(defun sbt-run-previous-command ()
  "Repeat the command that was previously executed (or run the
sbt:default-command, if no other command has yet been run)."
  (interactive)
  (sbt:command (sbt:get-previous-command)))

(defun sbt-completion-at-point ()
  "Complete the command at point. Works both in sbt shell and
scala console."
 (interactive) (sbt:completion-at-point))

(defun sbt-send-region (start end)
  "Send the selected region (between the mark and the current
point) to the sbt process of the current buffer's sbt
project. Whitespace and comments at the beginning or end of the
region are not sent."
  (interactive "r")
  (sbt:send-region start end))

(defun sbt-paste-region (&optional no-exit)
  "Send the selected region (between the mark and the current
point) to the sbt process of the current buffer's sbt project
using :paste REPL command.  Whitespace and comments at the
beginning or end of the region are not sent.  If the optional
NO-EXIT is non-zero, it will not exit the :paste session, so that
subsequent call to this function may provide additional input."
  (interactive "P")
  ;; TODO: Currently, NO-EXIT does not work correctly.
  ;; (sbt:paste-region (region-beginning) (region-end) arg)
  (sbt:paste-region (region-beginning) (region-end) nil))

(defun sbt-send-eol ()
  "Send newline to the sbt process for the primary purpose of
interrupting triggered execution, such as ~compile."
  (interactive)
  (comint-send-string (current-buffer) "\n"))

(defun sbt:clear (&optional buffer)
  "Clear (erase) the SBT buffer."
  (with-current-buffer (or buffer (sbt:buffer-name))
    (let ((proc (get-buffer-process (current-buffer)))
          (inhibit-read-only t))
      (ignore-errors (compilation-forget-errors))
      (erase-buffer)
      (ignore-errors (comint-send-string proc (kbd "C-l"))))))

(defun sbt:command (command &optional focus)
  (unless command (error "Please specify a command"))

  (unless (sbt:find-root)
    (error (concat "You're not in an sbt project.  "
		   "Maybe build.sbt or build.scala is missing?  "
		   "See http://ensime.org/build_tools")))

  (when (not (comint-check-proc (sbt:buffer-name)))
    (sbt:run-sbt))

  (when sbt:save-some-buffers
    (save-some-buffers
     (not compilation-ask-about-save)
     (sbt:buffer-in-project-function (sbt:find-root))))

  (with-current-buffer (sbt:buffer-name)
    (when sbt:display-command-buffer
      (if focus (pop-to-buffer (current-buffer)) (display-buffer (current-buffer))))
    (cond ((eq sbt:submode 'console)
           (comint-send-string (current-buffer) ":quit\n"))
          ((eq sbt:submode 'paste-mode)
           (comint-send-string (current-buffer)
                               (concat sbt:quit-paste-command
                                       ":quit\n"))))
    (if sbt:clear-buffer-before-command
        (sbt:clear (current-buffer))
      (ignore-errors (compilation-forget-errors)))
    (comint-send-string (current-buffer) (concat command "\n"))
    (setq next-error-last-buffer (current-buffer))))

(defun sbt:get-previous-command ()
  (if (not (get-buffer (sbt:buffer-name)))
      sbt:default-command
    (with-current-buffer (sbt:buffer-name)
      sbt:previous-command)))

(defun sbt:run-sbt (&optional kill-existing-p pop-p)
  "Start or re-strats (if kill-existing-p is non-NIL) sbt in a
buffer called *sbt*projectdir."
  (let* ((project-root (or (sbt:find-root)
			   (error "Could not find project root, type `C-h f sbt:find-root` for help.")))
         (buffer-name (sbt:buffer-name))
         ;; WORKAROUND https://github.com/jline/jline2/pull/285
         (process-environment (append '("EMACS=true") process-environment))
         (inhibit-read-only 1))
    ;; (when (null project-root)
    ;;   (error "Could not find project root, type `C-h f sbt:find-root` for help."))

    (when (not (or (executable-find sbt:program-name)
                   (file-executable-p (concat project-root sbt:program-name))))
      (error "Could not find %s in %s or on PATH. Please customize the sbt:program-name variable." sbt:program-name project-root))

    ;; kill existing sbt
    (when (and kill-existing-p (get-buffer buffer-name))
      (sbt:clear buffer-name)
      (kill-buffer buffer-name))

    ;; start new sbt
    (with-current-buffer (get-buffer-create buffer-name)
      (when pop-p (pop-to-buffer-same-window (current-buffer)))
      (unless (comint-check-proc (current-buffer))
        (unless (derived-mode-p 'sbt-mode) (sbt-mode))
        (cd project-root)
        (buffer-disable-undo)
        (message "Starting sbt in buffer %s " buffer-name)

        ;; insert a string to buffer so that process mark comes after
        ;; compilation-messages-start mark.
        (insert (concat "Running " sbt:program-name "\n"))
        (goto-char (point-min))
        (ignore-errors (compilation-forget-errors))
        (comint-exec (current-buffer) buffer-name sbt:program-name nil sbt:program-options))
      (current-buffer))))

(defun sbt:initialize-for-compilation-mode ()
  (setq-local
   compilation-directory-matcher
   '("--go-home-compile.el--you-are-drn^H^H^Hbugs--"))
  (setq-local
   compilation-error-regexp-alist
   '(;; scalac
     ("^\\[error][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):" 1 2 nil 2 1)
     ("^\\[warn][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):" 1 2 nil 1 1)
     ("^\\[info][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):" 1 2 nil 0 1)
     ;; failing scalatests
     ("^\\[info][[:space:]]+\\(.*\\) (\\([^:[:space:]]+\\):\\([[:digit:]]+\\))" 2 3 nil 2 1)
     ;; https://github.com/Duhemm/sbt-errors-summary
     ("^\\[error][[:space:]]\\[[[:digit:]]+][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):" 1 2 3 2 1)
     ("^\\[warn][[:space:]][[:space:]]\\[[[:digit:]]+][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):" 1 2 3 1 1)
     ))
  (setq-local compilation-mode-font-lock-keywords nil)
  (compilation-setup t))

(defvar sbt:mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map
                       (make-composed-keymap compilation-shell-minor-mode-map
                                             comint-mode-map))
    (define-key map (kbd "C-c C-j") 'sbt-send-eol)
    (define-key map (kbd "TAB") 'sbt-completion-at-point)
    (define-key map (kbd "C-c C-v") 'sbt-clear)

    map)
  "Basic mode map for `sbt-start'")

(defun sbt:compilation-parse-errors (start end &rest rules)
  "Since with compile.el it is impossible to parse scalac error message
with column information (since column indicator is on different line
then file name and row), we are going to use this :after advice for parsing
scalac output and update `compilation-message's in sbt buffer accordingly."
  (when (string-prefix-p sbt:buffer-name-base (buffer-name))
    (progn
      (goto-char start)
      (beginning-of-line)
      (while (re-search-forward "^\\[error][[:space:]]+^$" end t)
        (save-match-data
          (let* ((error-column (current-column))
                 (compilation-message-location (previous-single-property-change (point) 'compilation-message))
                 (compilation-message-property (get-text-property (1- compilation-message-location) 'compilation-message))
                 (compilation-message-loc (compilation--message->loc compilation-message-property)))
            ;; update only `compilation-message-loc' which do not have column information already
            (when (null (car compilation-message-loc))
              (setcar compilation-message-loc (- error-column 8)))))))))

(define-derived-mode sbt-mode comint-mode "sbt"
  "Major mode for `sbt-start'.

\\{sbt:mode-map}"
  (use-local-map sbt:mode-map)
  (ignore-errors (scala-mode:set-scala-syntax-mode))
  (add-hook 'sbt-mode-hook 'sbt:initialize-for-comint-mode)
  (add-hook 'sbt-mode-hook 'sbt:initialize-for-compilation-mode)
  (advice-add 'compilation-parse-errors :after #'sbt:compilation-parse-errors))

(provide 'sbt-mode)
;;; sbt-mode.el ends here
