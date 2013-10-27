;;; evil-ex.el --- Ex-mode

;; Author: Frank Fischer <frank fischer at mathematik.tu-chemnitz.de>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.0-dev

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Ex is implemented as an extensible minilanguage, whose grammar
;; is stored in `evil-ex-grammar'.  Ex commands are defined with
;; `evil-ex-define-cmd', which creates a binding from a string
;; to an interactive function.  It is also possible to define key
;; sequences which execute a command immediately when entered:
;; such shortcuts go in `evil-ex-map'.
;;
;; To provide buffer and filename completion, as well as interactive
;; feedback, Ex defines the concept of an argument handler, specified
;; with `evil-ex-define-argument-type'.  In the case of the
;; substitution command (":s/foo/bar"), the handler incrementally
;; highlights matches in the buffer as the substitution is typed.

(require 'evil-common)
(require 'evil-states)

;;; Code:

(defconst evil-ex-grammar
  '((expression
     (count command argument #'evil-ex-call-command)
     ((\? range) command argument #'evil-ex-call-command)
     (line #'evil-goto-line)
     (sexp #'eval-expression))
    (count
     number)
    (command #'evil-ex-parse-command)
    (binding
     "[~&*@<>=:]+\\|[[:alpha:]-]+\\|!")
    (bang
     (\? (! space) "!" #'$1))
    (argument
     ((\? space) (\? "\\(?:.\\|\n\\)+") #'$2))
    (range
     ("%" #'(evil-ex-full-range))
     (line (\? "[,;]" line #'$2) #'evil-ex-range)
     ("`" "[-a-zA-Z_<>']" ",`" "[-a-zA-Z_<>']"
      #'(evil-ex-char-marker-range $2 $4)))
    (line
     (base (\? offset) #'evil-ex-line)
     ((\? base) offset #'evil-ex-line))
    (base
     number
     marker
     search
     ("\\^" #'(evil-ex-first-line))
     ("\\$" #'(evil-ex-last-line))
     ("\\." #'(evil-ex-current-line)))
    (offset
     (+ signed-number #'+))
    (marker
     ("'" "[-a-zA-Z_<>']" #'(evil-ex-marker $2)))
    (search
     forward
     backward
     next
     prev
     subst)
    (forward
     ("/" "\\(?:[\\].\\|[^/,; ]\\)+" (! "/")
      #'(evil-ex-re-fwd $2))
     ("/" "\\(?:[\\].\\|[^/]\\)+" "/"
      #'(evil-ex-re-fwd $2)))
    (backward
     ("\\?" "\\(?:[\\].\\|[^?,; ]\\)+" (! "\\?")
      #'(evil-ex-re-bwd $2))
     ("\\?" "\\(?:[\\].\\|[^?]\\)+" "\\?"
      #'(evil-ex-re-bwd $2)))
    (next
     "\\\\/" #'(evil-ex-prev-search))
    (prev
     "\\\\\\?" #'(evil-ex-prev-search))
    (subst
     "\\\\&" #'(evil-ex-prev-search))
    (signed-number
     (sign (\? number) #'evil-ex-signed-number))
    (sign
     "\\+\\|-" #'intern)
    (number
     "[0-9]+" #'string-to-number)
    (space
     "[ ]+")
    (sexp
     "(.*)" #'(car-safe (read-from-string $1))))
  "Grammar for Ex.
An association list of syntactic symbols and their definitions.
The first entry is the start symbol.  A symbol's definition may
reference other symbols, but the grammar cannot contain
left recursion.  See `evil-parser' for a detailed explanation
of the syntax.")

(defun evil-ex-p ()
  "Whether Ex is currently active."
  (and evil-ex-current-buffer t))

(evil-define-command evil-ex (&optional initial-input)
  "Enter an Ex command."
  :keep-visual t
  (interactive
   (cond
    ((and (evil-visual-state-p)
          evil-ex-visual-char-range
          (memq (evil-visual-type) '(inclusive exclusive)))
     '("`<,`>"))
    ((evil-visual-state-p)
     '("'<,'>"))
    (current-prefix-arg
     (let ((arg (prefix-numeric-value current-prefix-arg)))
       (cond ((< arg 0) (setq arg (1+ arg)))
             ((> arg 0) (setq arg (1- arg))))
       (if (= arg 0) '(".")
         `(,(format ".,.%+d" arg)))))))
  (let ((evil-ex-current-buffer (current-buffer))
        (evil-ex-previous-command (unless initial-input
                                    (car-safe evil-ex-history)))
        evil-ex-argument-handler
        evil-ex-info-string
        result)
    (minibuffer-with-setup-hook
        #'evil-ex-setup
      (setq result
            (read-from-minibuffer
             ":"
             (or initial-input
                 (and evil-ex-previous-command
                      (format "(default: %s) " evil-ex-previous-command)))
             evil-ex-completion-map
             nil
             'evil-ex-history
             evil-ex-previous-command
             t)))
    ;; empty input means repeating the previous command
    (when (zerop (length result))
      (setq result evil-ex-previous-command))
    ;; parse data
    (evil-ex-update nil nil nil result)
    ;; execute command
    (unless (zerop (length result))
      (if evil-ex-expression
          (eval evil-ex-expression)
        (error "Ex: syntax error")))))

(defun evil-ex-delete-backward-char ()
  "Close the minibuffer if it is empty.
Otherwise behaves like `delete-backward-char'."
  (interactive)
  (call-interactively
   (if (zerop (length (minibuffer-contents)))
       #'abort-recursive-edit
     #'delete-backward-char)))

(defun evil-ex-setup ()
  "Initialize Ex minibuffer.
This function registers several hooks that are used for the
interactive actions during ex state."
  (add-hook 'after-change-functions #'evil-ex-update nil t)
  (add-hook 'minibuffer-exit-hook #'evil-ex-teardown)
  (when evil-ex-previous-command
    (add-hook 'pre-command-hook #'evil-ex-remove-default))
  (remove-hook 'minibuffer-setup-hook #'evil-ex-setup)
  (with-no-warnings
    (make-variable-buffer-local 'completion-at-point-functions))
  (setq completion-at-point-functions
        '(evil-ex-command-completion-at-point
          evil-ex-argument-completion-at-point)))
(put 'evil-ex-setup 'permanent-local-hook t)

(defun evil-ex-teardown ()
  "Deinitialize Ex minibuffer.
Clean up everything set up by `evil-ex-setup'."
  (remove-hook 'minibuffer-exit-hook #'evil-ex-teardown)
  (remove-hook 'after-change-functions #'evil-ex-update t)
  (when evil-ex-argument-handler
    (let ((runner (evil-ex-argument-handler-runner
                   evil-ex-argument-handler)))
      (when runner
        (funcall runner 'stop)))))
(put 'evil-ex-teardown 'permanent-local-hook t)

(defun evil-ex-remove-default ()
  "Remove the default text shown in the ex minibuffer.
When ex starts, the previous command is shown enclosed in
parenthesis. This function removes this text when the first key
is pressed."
  (delete-minibuffer-contents)
  (remove-hook 'pre-command-hook #'evil-ex-remove-default))
(put 'evil-ex-remove-default 'permanent-local-hook t)

(defun evil-ex-update (&optional beg end len string)
  "Update Ex variables when the minibuffer changes.
This function is usually called from `after-change-functions'
hook. If BEG is non-nil (which is the case when called from
`after-change-functions'), then an error description is shown
in case of incomplete or unknown commands."
  (let* ((prompt (minibuffer-prompt-end))
         (string (or string (buffer-substring prompt (point-max))))
         arg bang cmd count expr func handler range tree type)
    (cond
     ((and (eq this-command #'self-insert-command)
           (commandp (setq cmd (lookup-key evil-ex-map string))))
      (setq evil-ex-expression `(call-interactively #',cmd))
      (when (minibufferp)
        (exit-minibuffer)))
     (t
      (setq cmd nil)
      ;; store the buffer position of each character
      ;; as the `ex-index' text property
      (dotimes (i (length string))
        (add-text-properties
         i (1+ i) (list 'ex-index (+ i prompt)) string))
      (with-current-buffer evil-ex-current-buffer
        (setq tree (evil-ex-parse string t)
              expr (evil-ex-parse string))
        (when (eq (car-safe expr) 'evil-ex-call-command)
          (setq count (eval (nth 1 expr))
                cmd (eval (nth 2 expr))
                arg (eval (nth 3 expr))
                range (cond
                       ((evil-range-p count)
                        count)
                       ((numberp count)
                        (evil-ex-range count count)))
                bang (and (string-match ".!$" cmd) t))))
      (setq evil-ex-tree tree
            evil-ex-expression expr
            evil-ex-range range
            evil-ex-command cmd
            evil-ex-bang bang
            evil-ex-argument arg)
      ;; test the current command
      (when (and cmd (minibufferp))
        (setq func (evil-ex-completed-binding cmd t))
        (cond
         ;; update argument-handler
         (func
          (when (setq type (evil-get-command-property
                            func :ex-arg))
            (setq handler (cdr-safe
                           (assoc type
                                  evil-ex-argument-types))))
          (unless (eq handler evil-ex-argument-handler)
            (let ((runner (and evil-ex-argument-handler
                               (evil-ex-argument-handler-runner
                                evil-ex-argument-handler))))
              (when runner (funcall runner 'stop)))
            (setq evil-ex-argument-handler handler)
            (let ((runner (and evil-ex-argument-handler
                               (evil-ex-argument-handler-runner
                                evil-ex-argument-handler))))
              (when runner (funcall runner 'start evil-ex-argument))))
          (let ((runner (and evil-ex-argument-handler
                             (evil-ex-argument-handler-runner
                              evil-ex-argument-handler))))
            (when runner (funcall runner 'update evil-ex-argument))))
         (beg
          ;; show error message only when called from `after-change-functions'
          (let ((n (length (all-completions cmd (evil-ex-completion-table)))))
            (cond
             ((> n 1) (evil-ex-echo "Incomplete command"))
             ((= n 0) (evil-ex-echo "Unknown command")))))))))))
(put 'evil-ex-update 'permanent-local-hook t)

(defun evil-ex-echo (string &rest args)
  "Display a message after the current Ex command."
  (with-selected-window (minibuffer-window)
    (with-current-buffer (window-buffer (minibuffer-window))
      (unless (or evil-no-display
                  (zerop (length string)))
        (let ((string (format " [%s]" (apply #'format string args)))
              after-change-functions before-change-functions)
          (put-text-property 0 (length string) 'face 'evil-ex-info string)
          (minibuffer-message string))))))

(defun evil-ex-completion ()
  "Completes the current ex command or argument."
  (interactive)
  (let (after-change-functions)
    (evil-ex-update)
    (completion-at-point)
    (remove-text-properties (minibuffer-prompt-end) (point-max) '(face nil evil))))

(defun evil-ex-command-completion-at-point ()
  (let ((context (evil-ex-syntactic-context (1- (point)))))
    (when (memq 'command context)
      (let ((beg (or (get-text-property 0 'ex-index evil-ex-command)
                     (point)))
            (end (1+ (or (get-text-property (1- (length evil-ex-command))
                                            'ex-index
                                            evil-ex-command)
                         (1- (point))))))
        (when evil-ex-bang) (setq end (1+ end))
        (list beg end (evil-ex-completion-table))))))

(defun evil-ex-completion-table ()
  (cond
   ((eq evil-ex-complete-emacs-commands nil)
    #'evil-ex-command-collection)
   ((eq evil-ex-complete-emacs-commands 'in-turn)
    (completion-table-in-turn
     #'evil-ex-command-collection
     #'(lambda (str pred flag)
         (completion-table-with-predicate
          obarray #'commandp t str pred flag))))
   (t
    #'(lambda (str pred flag)
        (evil-completion-table-concat
         #'evil-ex-command-collection
         #'(lambda (str pred flag)
             (completion-table-with-predicate
              obarray #'commandp t str pred flag))
         str pred flag)))))

(defun evil-completion-table-concat (table1 table2 string pred flag)
  (cond
   ((eq flag nil)
    (let ((result1 (try-completion string table1 pred))
          (result2 (try-completion string table2 pred)))
      (cond
       ((null result1) result2)
       ((null result2) result1)
       ((and (eq result1 t) (eq result2 t)) t)
       (t (assert (equal result1 result2))
          result1))))
   ((eq flag t)
    (delete-dups
     (append (all-completions string table1 pred)
             (all-completions string table2 pred))))
   ((eq flag 'lambda)
    (and (or (eq t (test-completion string table1 pred))
             (eq t (test-completion string table2 pred)))
         t))
   ((eq (car-safe flag) 'boundaries)
    (or (completion-boundaries string table1 pred (cdr flag))
        (completion-boundaries string table2 pred (cdr flag))))
   ((eq flag 'metadata)
    '(metadata (display-sort-function . evil-ex-sort-completions)))))

(defun evil-ex-sort-completions (completions)
  (sort completions
        #'(lambda (str1 str2)
            (let ((p1 (eq 'evil-ex-commands (get-text-property 0 'face str1)))
                  (p2 (eq 'evil-ex-commands (get-text-property 0 'face str2))))
              (if (equal p1 p2)
                  (string< str1 str2)
                p1)))))

(defun evil-ex-command-collection (cmd predicate flag)
  "Called to complete a command."
  (let (commands)
    ;; append ! to all commands that may take a bang argument
    (dolist (cmd (mapcar #'car evil-ex-commands))
      (push cmd commands)
      (if (evil-ex-command-force-p cmd)
          (push (concat cmd "!") commands)))
    (when (eq evil-ex-complete-emacs-commands t)
      (setq commands
            (mapcar #'(lambda (str) (propertize str 'face 'evil-ex-commands))
                    commands)))
    (cond
     ((eq flag nil) (try-completion cmd commands predicate))
     ((eq flag t) (all-completions cmd commands predicate))
     ((eq flag 'lambda) (test-completion cmd commands))
     ((eq (car-safe flag) 'boundaries)
      `(boundaries 0 . ,(length (cdr flag)))))))

(defun evil-ex-argument-completion-at-point ()
  (let ((context (evil-ex-syntactic-context (1- (point)))))
    (when (memq 'argument context)
      (let* ((beg (or (and evil-ex-argument
                           (get-text-property 0 'ex-index evil-ex-argument))
                      (point)))
             (end (1+ (or (and evil-ex-argument
                               (get-text-property (1- (length evil-ex-argument))
                                                  'ex-index
                                                  evil-ex-argument))
                          (1- (point)))))
             (binding (evil-ex-completed-binding evil-ex-command))
             (arg-type (evil-get-command-property binding :ex-arg))
             (arg-handler (assoc arg-type evil-ex-argument-types))
             (completer (and arg-handler
                             (evil-ex-argument-handler-completer
                              (cdr arg-handler)))))
        (when completer
          (if (eq (car completer) 'collection)
              (list beg end (cdr completer))
            (save-restriction
              (narrow-to-region beg (point-max))
              (funcall (cdr completer)))))))))

(defun evil-ex-define-cmd (cmd function)
  "Binds the function FUNCTION to the command CMD."
  (if (string-match "^[^][]*\\(\\[\\(.*\\)\\]\\)[^][]*$" cmd)
      (let ((abbrev (replace-match "" nil t cmd 1))
            (full (replace-match "\\2" nil nil cmd 1)))
        (evil-add-to-alist 'evil-ex-commands full function)
        (evil-add-to-alist 'evil-ex-commands abbrev full))
    (evil-add-to-alist 'evil-ex-commands cmd function)))

(defun evil-ex-make-argument-handler (runner completer)
  (list runner completer))

(defun evil-ex-argument-handler-runner (arg-handler)
  (car arg-handler))

(defun evil-ex-argument-handler-completer (arg-handler)
  (cadr arg-handler))

(defmacro evil-ex-define-argument-type (arg-type doc &rest body)
  "Defines a new handler for argument-type ARG-TYPE.
DOC is the documentation string. It is followed by a list of
keywords and function:

:collection COLLECTION

  A collection for completion as required by `all-completions'.

:completion-at-point FUNC

  Function to be called to initialize a potential
  completion. FUNC must match the requirements as described for
  the variable `completion-at-point-functions'. When FUNC is
  called the minibuffer content is narrowed to exactly match the
  argument.

:runner FUNC

  Function to be called when the type of the current argument
  changes or when the content of this argument changes. This
  function should take one obligatory argument FLAG followed by
  an optional argument ARG. FLAG is one of three symbol 'start,
  'stop or 'update. When the argument type is recognized for the
  first time and this handler is started the FLAG is 'start. If
  the argument type changes to something else or ex state
  finished the handler FLAG is 'stop. If the content of the
  argument has changed FLAG is 'update. If FLAG is either 'start
  or 'update then ARG is the current value of this argument. If
  FLAG is 'stop then arg is nil."
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]])))
  (unless (stringp doc) (push doc body))
  (let (runner completer)
    (while (keywordp (car-safe body))
      (let ((key (pop body))
            (func (pop body)))
        (cond
         ((eq key :runner)
          (setq runner func))
         ((eq key :collection)
          (setq completer (cons 'collection func)))
         ((eq key :completion-at-point)
          (setq completer (cons 'completion-at-point func))))))
    `(eval-and-compile
       (evil-add-to-alist
        'evil-ex-argument-types
        ',arg-type
        '(,runner ,completer)))))

(evil-ex-define-argument-type file
  "Handles a file argument."
  :collection read-file-name-internal)

(evil-ex-define-argument-type buffer
  "Called to complete a buffer name argument."
  :collection internal-complete-buffer)

(declare-function shell-completion-vars "shell" ())

(defun evil-ex-init-shell-argument-completion (flag &optional arg)
  "Prepares the current minibuffer for completion of shell commands.
This function must be called from the :runner function of some
argument handler that requires shell completion."
  (when (and (eq flag 'start)
             (not evil-ex-shell-argument-initialized)
             (require 'shell nil t)
             (require 'comint nil t))
    (set (make-local-variable 'evil-ex-shell-argument-initialized) t)
    (cond
     ;; Emacs 24
     ((fboundp 'comint-completion-at-point)
      (shell-completion-vars))
     (t
      (set (make-local-variable 'minibuffer-default-add-function)
           'minibuffer-default-add-shell-commands)))
    (setq completion-at-point-functions
          '(evil-ex-command-completion-at-point
            evil-ex-argument-completion-at-point))))

;; because this variable is used only for Emacs 23 shell completion,
;; we put it here instead of "evil-vars.el"
(defvar evil-ex-shell-argument-range nil
  "Internal helper variable for Emacs 23 shell completion.")

(defun evil-ex-shell-command-completion-at-point ()
  "Completion at point function for shell commands."
  (cond
   ;; Emacs 24
   ((fboundp 'comint-completion-at-point)
    (comint-completion-at-point))
   ;; Emacs 23
   ((fboundp 'minibuffer-complete-shell-command)
    (set (make-local-variable 'evil-ex-shell-argument-range)
         (list (point-min) (point-max)))
    #'(lambda ()
        ;; We narrow the buffer to the argument so
        ;; `minibuffer-complete-shell-command' will correctly detect
        ;; the beginning of the argument.  When narrowing the buffer
        ;; to the argument the leading text in the minibuffer will be
        ;; hidden. Therefore we add a dummy overlay which shows that
        ;; text during narrowing.
        (let* ((beg (car evil-ex-shell-argument-range))
               (end (cdr evil-ex-shell-argument-range))
               (prev-text (buffer-substring
                           (point-min)
                           (car evil-ex-shell-argument-range)))
               (ov (make-overlay beg beg)))
          (overlay-put ov 'before-string prev-text)
          (save-restriction
            (apply #'narrow-to-region evil-ex-shell-argument-range)
            (minibuffer-complete-shell-command))
          (delete-overlay ov))))))

(evil-ex-define-argument-type shell
  "Shell argument type, supports completion."
  :completion-at-point evil-ex-shell-command-completion-at-point
  :runner evil-ex-init-shell-argument-completion)

(defun evil-ex-file-or-shell-command-completion-at-point ()
  (if (and (< (point-min) (point-max))
           (= (char-after (point-min)) ?!))
      (save-restriction
        (narrow-to-region (1+ (point-min)) (point-max))
        (evil-ex-shell-command-completion-at-point))
    (list (point-min) (point-max) #'read-file-name-internal)))

(evil-ex-define-argument-type file-or-shell
  "File or shell argument type.
If the current argument starts with a ! the rest of the argument
is considered a shell command, otherwise a file-name. Completion
works accordingly."
  :completion-at-point evil-ex-file-or-shell-command-completion-at-point
  :runner evil-ex-init-shell-argument-completion)

(defun evil-ex-binding (command &optional noerror)
  "Returns the final binding of COMMAND."
  (let ((binding command))
    (when binding
      (string-match "^\\(.+?\\)\\!?$" binding)
      (setq binding (match-string 1 binding))
      (while (progn
               (setq binding (cdr (assoc binding evil-ex-commands)))
               (stringp binding)))
      (unless binding
        (setq binding (intern command)))
      (if (commandp binding)
          binding
        (unless noerror
          (error "Unknown command: `%s'" command))))))

(defun evil-ex-completed-binding (command &optional noerror)
  "Returns the final binding of the completion of COMMAND."
  (let ((completion (try-completion command evil-ex-commands)))
    (evil-ex-binding (if (eq completion t) command
                       (or completion command))
                     noerror)))

;;; TODO: extensions likes :p :~ <cfile> ...
(defun evil-ex-replace-special-filenames (file-name)
  "Replace special symbols in FILE-NAME.
Replaces % by the current file-name,
Replaces # by the alternate file-name in FILE-NAME."
  (let ((current-fname (buffer-file-name))
        (alternate-fname (and (other-buffer)
                              (buffer-file-name (other-buffer)))))
    (when current-fname
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%\\)"
                                      current-fname file-name
                                      t t 2)))
    (when alternate-fname
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(#\\)"
                                      alternate-fname file-name
                                      t t 2)))
    (setq file-name
          (replace-regexp-in-string "\\\\\\([#%]\\)"
                                    "\\1" file-name t)))
  file-name)

(defun evil-ex-file-arg ()
  "Returns the current Ex argument as a file name.
This function interprets special file names like # and %."
  (unless (or (null evil-ex-argument)
              (zerop (length evil-ex-argument)))
    (evil-ex-replace-special-filenames evil-ex-argument)))

(defun evil-ex-repeat (count)
  "Repeats the last ex command."
  (interactive "P")
  (when count
    (goto-char (point-min))
    (forward-line (1- count)))
  (let ((evil-ex-current-buffer (current-buffer))
        (hist evil-ex-history))
    (while hist
      (let ((evil-ex-last-cmd (pop hist)))
        (when evil-ex-last-cmd
          (evil-ex-update nil nil nil evil-ex-last-cmd)
          (let ((binding (evil-ex-binding evil-ex-command)))
            (unless (eq binding #'evil-ex-repeat)
              (setq hist nil)
              (if evil-ex-expression
                  (eval evil-ex-expression)
                (error "Ex: syntax error")))))))))

(defun evil-ex-call-command (range command argument)
  "Execute the given command COMMAND."
  (let* ((count (when (numberp range) range))
         (range (when (evil-range-p range) range))
         (bang (and (string-match ".!$" command) t))
         (evil-ex-point (point))
         (evil-ex-range
          (or range (and count (evil-ex-range count count))))
         (evil-ex-command (evil-ex-completed-binding command))
         (evil-ex-bang (and bang t))
         (evil-ex-argument (copy-sequence argument))
         (evil-this-type (evil-type evil-ex-range))
         (current-prefix-arg count)
         (prefix-arg current-prefix-arg))
    (when (stringp evil-ex-argument)
      (set-text-properties
       0 (length evil-ex-argument) nil evil-ex-argument))
    (let ((buf (current-buffer)))
      (unwind-protect
          (if (not evil-ex-range)
              (call-interactively evil-ex-command)
            ;; set visual selection to match the region if an explicit
            ;; range has been specified
            (let ((ex-range (evil-copy-range evil-ex-range))
                  beg end)
              (evil-expand-range ex-range)
              (setq beg (evil-range-beginning ex-range)
                    end (evil-range-end ex-range))
              (evil-sort beg end)
              (set-mark end)
              (goto-char beg)
              (activate-mark)
              (call-interactively evil-ex-command)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (deactivate-mark)))))))

(defun evil-ex-line (base &optional offset)
  "Return the line number of BASE plus OFFSET."
  (+ (or base (line-number-at-pos))
     (or offset 0)))

(defun evil-ex-first-line ()
  "Return the line number of the first line."
  (line-number-at-pos (point-min)))

(defun evil-ex-current-line ()
  "Return the line number of the current line."
  (line-number-at-pos (point)))

(defun evil-ex-last-line ()
  "Return the line number of the last line."
  (save-excursion
    (goto-char (point-max))
    (when (bolp)
      (forward-line -1))
    (line-number-at-pos)))

(defun evil-ex-range (beg-line &optional end-line)
  "Returns the first and last position of the current range."
  (evil-range
   (evil-line-position beg-line)
   (evil-line-position (or end-line beg-line) -1)
   'line
   :expanded t))

(defun evil-ex-full-range ()
  "Return a range encompassing the whole buffer."
  (evil-range (point-min) (point-max) 'line))

(defun evil-ex-marker (marker)
  "Return MARKER's line number in the current buffer.
Signal an error if MARKER is in a different buffer."
  (when (stringp marker)
    (setq marker (aref marker 0)))
  (setq marker (evil-get-marker marker))
  (if (numberp marker)
      (line-number-at-pos marker)
    (error "Ex does not support markers in other files")))

(defun evil-ex-char-marker-range (beg end)
  (when (stringp beg) (setq beg (aref beg 0)))
  (when (stringp end) (setq end (aref end 0)))
  (setq beg (evil-get-marker beg)
        end (evil-get-marker end))
  (if (and (numberp beg) (numberp end))
      (evil-expand-range
       (evil-range beg end
                   (if (evil-visual-state-p)
                       (evil-visual-type)
                     'inclusive)))
    (error "Ex does not support markers in other files")))

(defun evil-ex-re-fwd (pattern)
  "Search forward for PATTERN.
Returns the line number of the match."
  (condition-case err
      (save-excursion
        (set-text-properties 0 (length pattern) nil pattern)
        (evil-move-end-of-line)
        (and (re-search-forward pattern nil t)
             (line-number-at-pos (1- (match-end 0)))))
    (invalid-regexp
     (evil-ex-echo (cadr err))
     nil)))

(defun evil-ex-re-bwd (pattern)
  "Search backward for PATTERN.
Returns the line number of the match."
  (condition-case err
      (save-excursion
        (set-text-properties 0 (length pattern) nil pattern)
        (evil-move-beginning-of-line)
        (and (re-search-backward pattern nil t)
             (line-number-at-pos (match-beginning 0))))
    (invalid-regexp
     (evil-ex-echo (cadr err))
     nil)))

(defun evil-ex-prev-search ()
  (error "Previous search not yet implemented"))

(defun evil-ex-signed-number (sign &optional number)
  "Return a signed number like -3 and +1.
NUMBER defaults to 1."
  (funcall sign (or number 1)))

(defun evil-ex-eval (string &optional start)
  "Evaluate STRING as an Ex command.
START is the start symbol, which defaults to `expression'."
  ;; disable the mark before executing, otherwise the visual region
  ;; may be used as operator range instead of the ex-range
  (let ((form (evil-ex-parse string nil start))
        transient-mark-mode deactivate-mark)
    (eval form)))

(defun evil-ex-parse (string &optional syntax start)
  "Parse STRING as an Ex expression and return an evaluation tree.
If SYNTAX is non-nil, return a syntax tree instead.
START is the start symbol, which defaults to `expression'."
  (let* ((start (or start (car-safe (car-safe evil-ex-grammar))))
         (match (evil-parser
                 string start evil-ex-grammar t syntax)))
    (car-safe match)))

(defun evil-ex-parse-command (string)
  "Parse STRING as an Ex binding."
  (let ((result (evil-parser string 'binding evil-ex-grammar))
        bang command)
    (when result
      (setq command (car-safe result)
            string (cdr-safe result))
      ;; parse a following "!" as bang only if
      ;; the command has the property :ex-bang t
      (when (evil-ex-command-force-p command)
        (setq result (evil-parser string 'bang evil-ex-grammar)
              bang (or (car-safe result) "")
              string (cdr-safe result)
              command (concat command bang)))
      (cons command string))))

(defun evil-ex-command-force-p (command)
  "Whether COMMAND accepts the bang argument."
  (let ((binding (evil-ex-completed-binding command t)))
    (when binding
      (evil-get-command-property binding :ex-bang))))

(defun evil-flatten-syntax-tree (tree)
  "Find all paths from the root of TREE to its leaves.
TREE is a syntax tree, i.e., all its leave nodes are strings.
The `nth' element in the result is the syntactic context
for the corresponding string index (counted from zero)."
  (let* ((result nil)
         (traverse nil)
         (traverse
          #'(lambda (tree path)
              (if (stringp tree)
                  (dotimes (char (length tree))
                    (push path result))
                (let ((path (cons (car tree) path)))
                  (dolist (subtree (cdr tree))
                    (funcall traverse subtree path)))))))
    (funcall traverse tree nil)
    (nreverse result)))

(defun evil-ex-syntactic-context (&optional pos)
  "Return the syntactical context of the character at POS.
POS defaults to the current position of point."
  (let* ((contexts (evil-flatten-syntax-tree evil-ex-tree))
         (length (length contexts))
         (pos (- (or pos (point)) (minibuffer-prompt-end))))
    (when (>= pos length)
      (setq pos (1- length)))
    (when (< pos 0)
      (setq pos 0))
    (when contexts
      (nth pos contexts))))

(defun evil-parser (string symbol grammar &optional greedy syntax)
  "Parse STRING as a SYMBOL in GRAMMAR.
If GREEDY is non-nil, the whole of STRING must match.
If the parse succeeds, the return value is a cons cell
\(RESULT . TAIL), where RESULT is a parse tree and TAIL is
the remainder of STRING. Otherwise, the return value is nil.

GRAMMAR is an association list of symbols and their definitions.
A definition is either a list of production rules, which are
tried in succession, or a #'-quoted function, which is called
to parse the input.

A production rule can be one of the following:

    nil matches the empty string.
    A regular expression matches a substring.
    A symbol matches a production for that symbol.
    (X Y) matches X followed by Y.
    (\\? X) matches zero or one of X.
    (* X) matches zero or more of X.
    (+ X) matches one or more of X.
    (& X) matches X, but does not consume.
    (! X) matches anything but X, but does not consume.

Thus, a simple grammar may look like:

    ((plus \"\\\\+\")           ; plus <- \"+\"
     (minus \"-\")            ; minus <- \"-\"
     (operator plus minus)) ; operator <- plus / minus

All input-consuming rules have a value. A regular expression evaluates
to the text matched, while a list evaluates to a list of values.
The value of a list may be overridden with a semantic action, which is
specified with a #'-quoted expression at the end:

    (X Y #'foo)

The value of this rule is the result of calling foo with the values
of X and Y as arguments. Alternatively, the function call may be
specified explicitly:

    (X Y #'(foo $1 $2))

Here, $1 refers to X and $2 refers to Y. $0 refers to the whole list.
Dollar expressions can also be used directly:

    (X Y #'$1)

This matches X followed by Y, but ignores the value of Y;
the value of the list is the same as the value of X.

If the SYNTAX argument is non-nil, then all semantic actions
are ignored, and a syntax tree is constructed instead. The
syntax tree obeys the property that all the leave nodes are
parts of the input string. Thus, by traversing the syntax tree,
one can determine how each character was parsed.

The following symbols have reserved meanings within a grammar:
`\\?', `*', `+', `&', `!', `function', `alt', `seq' and nil."
  (let ((string (or string ""))
        func pair result rules tail)
    (cond
     ;; epsilon
     ((member symbol '("" nil))
      (setq pair (cons (if syntax "" nil) string)))
     ;; token
     ((stringp symbol)
      (save-match-data
        (when (or (eq (string-match symbol string) 0)
                  ;; ignore leading whitespace
                  (and (eq (string-match "^[ \f\t\n\r\v]+" string) 0)
                       (eq (match-end 0)
                           (string-match
                            symbol string (match-end 0)))))
          (setq result (match-string 0 string)
                tail (substring string (match-end 0))
                pair (cons result tail))
          (when (and syntax pair)
            (setq result (substring string 0
                                    (- (length string)
                                       (length tail))))
            (setcar pair result)))))
     ;; symbol
     ((symbolp symbol)
      (let ((context symbol))
        (setq rules (cdr-safe (assq symbol grammar)))
        (setq pair (evil-parser string `(alt ,@rules)
                                grammar greedy syntax))
        (when (and syntax pair)
          (setq result (car pair))
          (if (and (listp result) (sequencep (car result)))
              (setq result `(,symbol ,@result))
            (setq result `(,symbol ,result)))
          (setcar pair result))))
     ;; function
     ((eq (car-safe symbol) 'function)
      (setq symbol (cadr symbol)
            pair (funcall symbol string))
      (when (and syntax pair)
        (setq tail (or (cdr pair) "")
              result (substring string 0
                                (- (length string)
                                   (length tail))))
        (setcar pair result)))
     ;; list
     ((listp symbol)
      (setq rules symbol
            symbol (car-safe rules))
      (if (memq symbol '(& ! \? * + alt seq))
          (setq rules (cdr rules))
        (setq symbol 'seq))
      (when (and (memq symbol '(+ alt seq))
                 (> (length rules) 1))
        (setq func (car (last rules)))
        (if (eq (car-safe func) 'function)
            (setq rules (delq func (copy-sequence rules))
                  func (cadr func))
          (setq func nil)))
      (cond
       ;; positive lookahead
       ((eq symbol '&)
        (when (evil-parser string rules grammar greedy syntax)
          (setq pair (evil-parser string nil grammar nil syntax))))
       ;; negative lookahead
       ((eq symbol '!)
        (unless (evil-parser string rules grammar greedy syntax)
          (setq pair (evil-parser string nil grammar nil syntax))))
       ;; zero or one
       ((eq symbol '\?)
        (setq rules (if (> (length rules) 1)
                        `(alt ,rules nil)
                      `(alt ,@rules nil))
              pair (evil-parser string rules grammar greedy syntax)))
       ;; zero or more
       ((eq symbol '*)
        (setq rules `(alt (+ ,@rules) nil)
              pair (evil-parser string rules grammar greedy syntax)))
       ;; one or more
       ((eq symbol '+)
        (let (current results)
          (catch 'done
            (while (setq current (evil-parser
                                  string rules grammar nil syntax))
              (setq result (car-safe current)
                    tail (or (cdr-safe current) "")
                    results (append results (if syntax result
                                              (cdr-safe result))))
              ;; stop if stuck
              (if (equal string tail)
                  (throw 'done nil)
                (setq string tail))))
          (when results
            (setq func (or func 'list)
                  pair (cons results tail)))))
       ;; alternatives
       ((eq symbol 'alt)
        (catch 'done
          (dolist (rule rules)
            (when (setq pair (evil-parser
                              string rule grammar greedy syntax))
              (throw 'done pair)))))
       ;; sequence
       (t
        (setq func (or func 'list))
        (let ((last (car-safe (last rules)))
              current results rule)
          (catch 'done
            (while rules
              (setq rule (pop rules)
                    current (evil-parser string rule grammar
                                         (when greedy
                                           (null rules))
                                         syntax))
              (cond
               ((null current)
                (setq results nil)
                (throw 'done nil))
               (t
                (setq result (car-safe current)
                      tail (cdr-safe current))
                (unless (memq (car-safe rule) '(& !))
                  (if (and syntax
                           (or (null result)
                               (and (listp result)
                                    (listp rule)
                                    ;; splice in single-element
                                    ;; (\? ...) expressions
                                    (not (and (eq (car-safe rule) '\?)
                                              (eq (length rule) 2))))))
                      (setq results (append results result))
                    (setq results (append results (list result)))))
                (setq string (or tail ""))))))
          (when results
            (setq pair (cons results tail))))))
      ;; semantic action
      (when (and pair func (not syntax))
        (setq result (car pair))
        (let* ((dexp
                #'(lambda (obj)
                    (when (symbolp obj)
                      (let ((str (symbol-name obj)))
                        (when (string-match "\\$\\([0-9]+\\)" str)
                          (string-to-number (match-string 1 str)))))))
               ;; traverse a tree for dollar expressions
               (dval nil)
               (dval
                #'(lambda (obj)
                    (if (listp obj)
                        (mapcar dval obj)
                      (let ((num (funcall dexp obj)))
                        (if num
                            (if (not (listp result))
                                result
                              (if (eq num 0)
                                  `(list ,@result)
                                (nth (1- num) result)))
                          obj))))))
          (cond
           ((null func)
            (setq result nil))
           ;; lambda function
           ((eq (car-safe func) 'lambda)
            (if (memq symbol '(+ seq))
                (setq result `(funcall ,func ,@result))
              (setq result `(funcall ,func ,result))))
           ;; string replacement
           ((or (stringp func) (stringp (car-safe func)))
            (let* ((symbol (or (car-safe (cdr-safe func))
                               (and (boundp 'context) context)
                               (car-safe (car-safe grammar))))
                   (string (if (stringp func) func (car-safe func))))
              (setq result (car-safe (evil-parser string symbol grammar
                                                  greedy syntax)))))
           ;; dollar expression
           ((funcall dexp func)
            (setq result (funcall dval func)))
           ;; function call
           ((listp func)
            (setq result (funcall dval func)))
           ;; symbol
           (t
            (if (memq symbol '(+ seq))
                (setq result `(,func ,@result))
              (setq result `(,func ,result))))))
        (setcar pair result))))
    ;; weed out incomplete matches
    (when pair
      (if (not greedy) pair
        (if (null (cdr pair)) pair
          ;; ignore trailing whitespace
          (when (string-match "^[ \f\t\n\r\v]*$" (cdr pair))
            (unless syntax (setcdr pair nil))
            pair))))))

(provide 'evil-ex)

;;; evil-ex.el ends here
