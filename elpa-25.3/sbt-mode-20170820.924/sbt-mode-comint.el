;;; sbt-mode-comint.el - Support functions for comint-mode
;;
;; Copyright(c) 2013 Heikki Vesalainen
;; For information on the License, see the LICENSE file

(require 'ansi-color)
(require 'cl-lib)
(require 'comint)
(require 'sbt-mode-vars)
(require 'sbt-mode-project)
(require 'sbt-mode-buffer)

(eval-when-compile
  (defvar sbt:previous-history-file)
  (defvar sbt:submode)
  (defun sbt-command (c)))

(defcustom sbt:sbt-history-file "target/.history"
  "The .history file written by sbt. Relative to the sbt project
root. This will be loaded as the comint-input-ring on start-up"
  :type 'string
  :group 'sbt)

(defcustom sbt:console-history-file "~/.scala_history"
  "The .scala_history file written by scala. This will be loaded
as the comint-input-ring on console start-up"
  :type 'string
  :group 'sbt)

(defcustom sbt:sbt-prompt-regexp "^\\(\\[[^\]]*\\] \\)?[>$][ ]*"
  "A regular expression to match sbt REPL prompt"
  :type 'string
  :group 'sbt)

(defcustom sbt:console-prompt-regexp "^scala>[ ]*"
  "A regular expression to match scala REPL prompt"
  :type 'string
  :group 'sbt)

(defcustom sbt:paste-mode-prompt-regexp "^// Entering paste mode"
  "A regular expression to detect paste-mode"
  :type 'string
  :group 'sbt)

(defcustom sbt:prompt-regexp "^\\(\\(scala\\|\\[[^\]]*\\] \\)?[>$]\\|[ ]+|\\)[ ]*"
  "A regular expression to match sbt and scala console prompts"
  :type 'string
  :group 'sbt)

(defcustom sbt:ansi-support t
  "See `ansi-color-for-comint-mode' in `ansi-color.el'"
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Filter" filter)
                 (const :tag "Translate" t))
  :group 'sbt)

(defcustom sbt:scroll-to-bottom-on-output nil
  "If `t' will always scroll sbt buffer to the bottom on insertion of a new output.
If `nil' will stop scrolling on a first error encountered or if point is not on last
line of output buffer."
  :type 'boolean
  :group 'sbt)

(defvar sbt:quit-paste-command (if (eq system-type 'windows-nt)
                                   (kbd "C-z")
                                 (kbd "C-d"))
  "Keys for sending quit command")

(defun sbt:initialize-for-comint-mode ()
  (sbt:require-buffer)
  (when (derived-mode-p 'comint-mode)

    (setq comint-process-echoes t)
    (setq comint-scroll-to-bottom-on-output sbt:scroll-to-bottom-on-output)
    (setq comint-prompt-regexp sbt:prompt-regexp)
    (setq-local comint-use-prompt-regexp t)
    (setq-local comint-prompt-read-only t)
    (setq-local comint-buffer-maximum-size 4096)
    (setq-local comint-preoutput-filter-functions '(sbt:move-marker-before-prompt-filter))
    (setq-local comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom sbt:move-marker-after-prompt-filter))
    (setq ansi-color-for-comint-mode sbt:ansi-support)
    (setq comint-input-sender 'sbt:input-sender)
    (setq-local sbt:previous-history-file nil)
    (setq-local sbt:submode nil)
    (add-hook 'comint-output-filter-functions 'sbt:switch-submode)
    (add-hook 'comint-output-filter-functions 'sbt:ansi-filter)
))

(defconst sbt:ansi-clear-line "M\\[2K"
  "'Ansi code' sequence sent by JLine to clear the previous
line.")

(defun sbt:input-sender (proc string)
  (sit-for 0) ; the purpose of this sit-for 0 is to let emacs show the
              ; newline that the user just inserted. Without this
              ; sometimes emacs will not give the user any feedback
              ; that the input has been sent.
  (comint-simple-send proc string))

(defun sbt:ansi-filter (input)
  (when (sbt:mode-p)
    (save-excursion
      ;; go to start of first line just inserted
      (comint-goto-process-mark)
      (goto-char (max (point-min) (- (point) (string-width input))))
      (forward-line 0)
      (while (re-search-forward sbt:ansi-clear-line nil t)
        ;; delete the ansi code and the previous line
        (delete-region (save-excursion (forward-line -1) (point)) (match-end 0))))
    input))

(defun sbt:move-marker-before-prompt-filter (input-string)
  "Move the process marker to beginning of prompt so that the
prompt will be moved with output. Also mangles the `input-string`
so that if it contains the prompt, it is moved to the end of the
input. This is needed because, especially in sbt, the output can
contain out-of-band output from other Threads that mix up the
prompt."

  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char pmark)
      (forward-line 0) ;; start of line
      (when (looking-at sbt:console-prompt-regexp)
        (set-marker pmark (- (point) 1)))))

  (let ((new-input-string 
         (if (string-match sbt:console-prompt-regexp input-string)
             (let* ((beg (match-beginning 0))
                    (before (substring input-string 0 (max 0 (- beg 1))))
                    (nl (substring input-string (max 0 (- beg 1)) beg))
                    (after (substring input-string (match-end 0)))
                    (prompt (match-string 0 input-string)))
               (concat before after nl prompt))
           input-string)))
    new-input-string))

(defun sbt:move-marker-after-prompt-filter (input-string)
  "Move the process marker to after prompt. This just reverses
what `sbt:move-marker-before-prompt-filter` did."

  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char pmark)
      (forward-line 1)
      (when (looking-at sbt:console-prompt-regexp)
        (set-marker pmark (match-end 0))))))

(defun sbt:switch-submode (input)
  (when (sbt:mode-p)
    (let ((submode
           (save-excursion
             (save-match-data
               ;; go to start of last line with text inserted by comint
               (comint-goto-process-mark)
               (skip-chars-backward " \n\r\t")
               (forward-line 0)
               (cond ((looking-at sbt:sbt-prompt-regexp) 'sbt)
                     ((looking-at sbt:console-prompt-regexp) 'console)
                     ((looking-at sbt:paste-mode-prompt-regexp) 'paste-mode))))))
      (when submode
        (setq sbt:submode submode)
        (setq comint-use-prompt-regexp (not (eq submode 'paste-mode)))

        (let ((comint-input-history-ignore "^completions\\|// completions$")
              (comint-input-ring-file-name
               (cond ((eq submode 'sbt) sbt:sbt-history-file)
                     ((eq submode 'console) sbt:console-history-file))))
          (when (and comint-input-ring-file-name
                     (not (equal comint-input-ring-file-name sbt:previous-history-file)))
            (setq sbt:previous-history-file comint-input-ring-file-name)
            (comint-read-input-ring)))))
    input))

;;;
;;; Completion functionality
;;;

(defun sbt:scala-escape-char (c)
  (cond ((= c ?\") "\\\"")
        ((= c ?\\) "\\\\")
        ((or (< c #x20) (> c #x7e)) (format "\\u%04x" c))
        (t (string c))))

(defun sbt:scala-escape-string (str)
  (mapconcat 'sbt:scala-escape-char str ""))

(defconst sbt:completions-regex "^\\[completions\\] \\(.*\\)?$")
(defconst sbt:repl-completions-string
  ;; waiting for better times... maybe some day we will have a completions command in
  ;; the scala-console
  (concat "{ val input = \"$1\"; "
          "val completer = new scala.tools.nsc.interpreter.JLineCompletion($intp).completer; "
          "val completions = completer.complete(input, input.length); "
          "val prefix = input.substring(0, completions.cursor); "
          "completions.candidates.foreach(c => println(\"[completions] \" + c))} // completions")
  "A command to send to scala console to get completions for $1 (an escaped string).")

(defun sbt:get-sbt-completions (input)
   (sbt:require-buffer)
   (when (not (comint-check-proc (current-buffer)))
     (error "sbt is not running in buffer %s" (current-buffer)))
   (when (save-excursion
           (comint-goto-process-mark)
           (beginning-of-line)
           (not (looking-at-p sbt:sbt-prompt-regexp)))
     (error "sbt is not ready (no prompt found)"))
   (message "Querying sbt for completions for %s..." input)
   (when (or (null input) (string-match "^\\s *$" input))
     (setq input ""))
   (setq input (concat "completions \""
                       (sbt:scala-escape-string input)
                       "\""))
   (prog1
       (comint-redirect-results-list input
                                     sbt:completions-regex
                                     1)
     (message nil)))

(defun sbt:get-console-completions (input)
   (sbt:require-buffer)
   (when (not (comint-check-proc (current-buffer)))
     (error "sbt is not running in buffer %s" (current-buffer)))
   (when (save-excursion
           (comint-goto-process-mark)
           (beginning-of-line)
           (not (looking-at-p sbt:console-prompt-regexp)))
     (error "scala console is not ready (no prompt found)"))
   (message "Querying scala console for completions for %s..." input)
   (setq input (replace-regexp-in-string "\\$1"
                                         (sbt:scala-escape-string input)
                                         sbt:repl-completions-string t t))
   (prog1
       (comint-redirect-results-list input
                                     sbt:completions-regex
                                     1)
     (message nil)))

(defun sbt:completion-at-point ()
  (sbt:require-buffer)
  (let ((point (point))
        (beg (save-excursion (comint-goto-process-mark)
                             (point)))
        (end (max (point)
                  (save-excursion (end-of-line)
                                  (skip-chars-backward " \t\n\r")
                                  (point))))
        mid)
    (goto-char beg)
    (beginning-of-line)
    (if (> beg end)
        (comint-goto-process-mark)
      (cond ((looking-at-p sbt:sbt-prompt-regexp)
             (goto-char point)
             (let ((completions (sbt:get-sbt-completions (buffer-substring beg end))))
               (completion-in-region beg end completions `(lambda (s) (> (string-width s) 0)))))
            ((looking-at-p sbt:console-prompt-regexp)
             (goto-char point)
             (save-excursion
               (goto-char end)
               ;; find mid point (point respective to which the completions are given
               (unless (or (= (point) beg) (looking-back "[.,;]" (1- (point))))
                 (backward-sexp))
               (setq mid (max beg (point)))
               ;; find beg point (point respective to which completions are requested
               (ignore-errors (while (> (point) beg) (backward-sexp)))
               (setq beg (max beg (point))))
             (let ((completions (sbt:get-console-completions (buffer-substring beg end))))
               (completion-in-region mid end completions `(lambda (s) (> (string-width s) 0)))))
            (t
             (goto-char point)
             "No sbt or scala prompt found before process mark")))))

(defun sbt:send-region (start end)
  (unless (comint-check-proc (sbt:buffer-name))
    (error "sbt is not running in buffer %s" (sbt:buffer-name)))
  (save-excursion
    (goto-char end)
    (skip-syntax-forward ">")
    (forward-comment (- (point-max)))
    (setq end (point)))
  (save-excursion
    (goto-char start)
    (forward-comment (point-max))
    (setq start (point)))
  (unless (> end start) (error "mark a region of code first"))
  (display-buffer (sbt:buffer-name))
  (let ((submode (buffer-local-value 'sbt:submode
                                     (get-buffer (sbt:buffer-name)))))
    (unless (or (eq submode 'console) (eq submode 'paste-mode))
      (sbt-command "console")))
  ;; TODO: Do not send region if there is an error.
  ;;
  ;; There may be compilation by (sbt-command "console")
  (let ((submode (buffer-local-value 'sbt:submode
                                     (get-buffer (sbt:buffer-name)))))
    (message "submode: %s" submode)
    (when (or (eq submode 'console) (eq submode 'paste-mode))
      (comint-send-region (sbt:buffer-name) start end)
      (comint-send-string (sbt:buffer-name) "\n"))))


(defun sbt:paste-region (start end &optional no-exit)
  "Send region (from START to END) using :paste REPL command.

If NO-EXIT is non-zero, this function will not end the paste
mode."
  (unless (comint-check-proc (sbt:buffer-name))
    (error "sbt is not running in buffer %s" (sbt:buffer-name)))
  (save-excursion
    (goto-char end)
    (skip-syntax-forward ">")
    (forward-comment (- (point-max)))
    (setq end (point)))
  (save-excursion
    (goto-char start)
    (forward-comment (point-max))
    (setq start (point)))
  (unless (> end start) (error "mark a region of code first"))
  (display-buffer (sbt:buffer-name))
  (let ((submode (buffer-local-value 'sbt:submode
                                     (get-buffer (sbt:buffer-name)))))
    (when (eq submode 'sbt)
      (sbt-command "console")))
  ;; TODO: verify if we entered "console" mode successfully.
  (comint-send-string (sbt:buffer-name) ":paste\n")

  (comint-send-region (sbt:buffer-name) start end)
  (comint-send-string (sbt:buffer-name) "\n")
  (unless no-exit
    (comint-send-string (sbt:buffer-name) sbt:quit-paste-command)))

(provide 'sbt-mode-comint)
