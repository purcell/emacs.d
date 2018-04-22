;;; inf-ruby.el --- Run a Ruby process in a buffer

;; Copyright (C) 1999-2008 Yukihiro Matsumoto, Nobuyoshi Nakada

;; Author: Yukihiro Matsumoto
;;         Nobuyoshi Nakada
;;         Cornelius Mika <cornelius.mika@gmail.com>
;;         Dmitry Gutov <dgutov@yandex.ru>
;;         Kyle Hargraves <pd@krh.me>
;; URL: http://github.com/nonsequitur/inf-ruby
;; Package-Version: 20170615.335
;; Created: 8 April 1998
;; Keywords: languages ruby
;; Version: 2.5.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; inf-ruby provides a REPL buffer connected to a Ruby subprocess.
;;
;; If you're installing manually, you'll need to:
;; * drop the file somewhere on your load path (perhaps ~/.emacs.d)
;; * Add the following lines to your .emacs file:
;;
;;    (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;;    (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
;;
;; Or, for enh-ruby-mode:
;;
;;    (add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
;;
;; Installation via ELPA interface does the above for you
;; automatically.
;;
;; Additionally, consider adding
;;
;;    (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
;;
;; to your init file to automatically switch from common Ruby compilation
;; modes to interact with a debugger.
;;
;; To call `inf-ruby-console-auto' more easily, you can, for example,
;; replace the original `inf-ruby' binding:
;;
;;   (eval-after-load 'inf-ruby
;;     '(define-key inf-ruby-minor-mode-map
;;        (kbd "C-c C-s") 'inf-ruby-console-auto))

;;; Code:

(require 'comint)
(require 'compile)
(require 'ruby-mode)
(require 'thingatpt)

(eval-when-compile
  (defvar rspec-compilation-mode-map)
  (defvar ruby-compilation-mode-map)
  (defvar projectile-rails-server-mode-map))

(defgroup inf-ruby nil
  "Run Ruby process in a buffer"
  :group 'languages)

(defcustom inf-ruby-prompt-read-only t
  "If non-nil, the prompt will be read-only.

Also see the description of `ielm-prompt-read-only'."
  :type 'boolean
  :group 'inf-ruby)

(defcustom inf-ruby-implementations
  '(("ruby"     . "irb --prompt default --noreadline -r irb/completion")
    ("jruby"    . "jruby -S irb --prompt default --noreadline -r irb/completion")
    ("rubinius" . "rbx -r irb/completion")
    ("yarv"     . "irb1.9 -r irb/completion")
    ("macruby"  . "macirb -r irb/completion")
    ("pry"      . "pry"))
  "An alist of ruby implementations to irb executable names."
  :type '(repeat (cons string string))
  :group 'inf-ruby)

(defcustom inf-ruby-default-implementation "ruby"
  "Which Ruby implementation to use if none is specified."
  :type `(choice ,@(mapcar (lambda (item) (list 'const (car item)))
                           inf-ruby-implementations))
  :group 'inf-ruby)

(defcustom inf-ruby-console-environment 'ask
  "Envronment to use for the `inf-ruby-console-*' commands.
If the value is not a string, ask the user to choose from the
available ones.  Otherwise, just use the value.

Currently only affects Rails and Hanami consoles."
  :type '(choice
          (const ask :tag "Ask the user")
          (string :tag "Environment name")))

(defconst inf-ruby-prompt-format
  (concat
   (mapconcat
    #'identity
    '("\\(^%s> *\\)"                      ; Simple
      "\\(^(rdb:1) *\\)"                  ; Debugger
      "\\(^(byebug) *\\)"                 ; byebug
      "\\(^\\(irb([^)]+)"                 ; IRB default
      "\\([[0-9]+] \\)?[Pp]ry ?([^)]+)"   ; Pry
      "\\(jruby-\\|JRUBY-\\)?[1-9]\\.[0-9]\\(\\.[0-9]+\\)*\\(-?p?[0-9]+\\)?" ; RVM
      "^rbx-head\\)")                     ; RVM continued
    "\\|")
   ;; Statement and nesting counters, common to the last four.
   " ?[0-9:]* ?%s *\\)")
  "Format string for the prompt regexp pattern.
Two placeholders: first char in the Simple prompt, and the last
graphical char in all other prompts.")

(defvar inf-ruby-first-prompt-pattern (format inf-ruby-prompt-format ">" ">")
  "First prompt regex pattern of Ruby interpreter.")

(defvar inf-ruby-prompt-pattern (format inf-ruby-prompt-format "[?>]" "[\]>*\"'/`]")
  "Prompt regex pattern of Ruby interpreter.")

(defvar inf-ruby-mode-hook nil
  "Hook for customizing `inf-ruby-mode'.")

(defvar inf-ruby-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "C-c C-l") 'ruby-load-file)
    (define-key map (kbd "C-x C-e") 'ruby-send-last-sexp)
    (define-key map (kbd "TAB") 'completion-at-point)
    (define-key map (kbd "C-x C-q") 'inf-ruby-maybe-switch-to-compilation)
    (define-key map (kbd "C-c C-z") 'ruby-switch-to-last-ruby-buffer)
    map)
  "Mode map for `inf-ruby-mode'.")

;;;###autoload
(defvar ruby-source-modes '(ruby-mode enh-ruby-mode)
  "Used to determine if a buffer contains Ruby source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a ruby source file by `ruby-load-file'.
Used by these commands to determine defaults.")

(defvar ruby-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.
Caches the last pair used in the last `ruby-load-file' command.
Used for determining the default in the
next one.")

(defvar inf-ruby-at-top-level-prompt-p t)
(make-variable-buffer-local 'inf-ruby-at-top-level-prompt-p)

(defvar inf-ruby-last-prompt nil)
(make-variable-buffer-local 'inf-ruby-last-prompt)

(defconst inf-ruby-error-regexp-alist
  '(("SyntaxError: \\(?:compile error\n\\)?\\([^\(].*\\):\\([1-9][0-9]*\\):" 1 2)
    ("^\tfrom \\([^\(].*\\):\\([1-9][0-9]*\\)\\(:in `.*'\\)?$" 1 2)))

;;;###autoload
(defun inf-ruby-setup-keybindings ()
  "Hook up `inf-ruby-minor-mode' to each of `ruby-source-modes'."
  (warn "`inf-ruby-setup-keybindings' is deprecated, please don't use it anymore.")
  (warn "If you're using `inf-ruby' from Git, please look up the new usage instructions."))

(make-obsolete 'inf-ruby-setup-keybindings 'add-hook "2.3.1")

(defvar inf-ruby-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x") 'ruby-send-definition)
    (define-key map (kbd "C-x C-e") 'ruby-send-last-sexp)
    (define-key map (kbd "C-c C-b") 'ruby-send-block)
    (define-key map (kbd "C-c M-b") 'ruby-send-block-and-go)
    (define-key map (kbd "C-c C-x") 'ruby-send-definition)
    (define-key map (kbd "C-c M-x") 'ruby-send-definition-and-go)
    (define-key map (kbd "C-c C-r") 'ruby-send-region)
    (define-key map (kbd "C-c M-r") 'ruby-send-region-and-go)
    (define-key map (kbd "C-c C-z") 'ruby-switch-to-inf)
    (define-key map (kbd "C-c C-l") 'ruby-load-file)
    (define-key map (kbd "C-c C-s") 'inf-ruby)
    (easy-menu-define
      inf-ruby-minor-mode-menu
      map
      "Inferior Ruby Minor Mode Menu"
      '("Inf-Ruby"
        ;; TODO: Add appropriate :active (or ENABLE) conditions.
        ["Send definition" ruby-send-definition t]
        ["Send last expression" ruby-send-last-sexp t]
        ["Send block" ruby-send-block t]
        ["Send region" ruby-send-region t]
        "--"
        ["Load file..." ruby-load-file t]
        "--"
        ["Start REPL" inf-ruby t]
        ["Switch to REPL" ruby-switch-to-inf t]
        ))
    map))

;;;###autoload
(define-minor-mode inf-ruby-minor-mode
  "Minor mode for interacting with the inferior process buffer.

The following commands are available:

\\{inf-ruby-minor-mode-map}"
  :lighter "" :keymap inf-ruby-minor-mode-map)

(defvar inf-ruby-buffer nil "The oldest live Ruby process buffer.")

(defvar inf-ruby-buffers nil "List of Ruby process buffers.")

(defvar inf-ruby-buffer-command nil "The command used to run Ruby shell")
(make-variable-buffer-local 'inf-ruby-buffer-command)

(defvar inf-ruby-buffer-impl-name nil "The name of the Ruby shell")
(make-variable-buffer-local 'inf-ruby-buffer-impl-name)

(define-derived-mode inf-ruby-mode comint-mode "Inf-Ruby"
  "Major mode for interacting with an inferior Ruby REPL process.

A simple IRB process can be fired up with \\[inf-ruby].

To launch a REPL with project-specific console instead, type
\\[inf-ruby-console-auto].  It recognizes several
project types, including Rails, gems and anything with `racksh'
in their Gemfile.

Customization: When entered, this mode runs `comint-mode-hook' and
`inf-ruby-mode-hook' (in that order).

You can send text to the inferior Ruby process from other buffers containing
Ruby source.

    `ruby-switch-to-inf' switches the current buffer to the ruby process buffer.
    `ruby-send-definition' sends the current definition to the ruby process.
    `ruby-send-region' sends the current region to the ruby process.
    `ruby-send-definition-and-go' and `ruby-send-region-and-go'
        switch to the ruby process buffer after sending their text.

Commands:
`RET' after the end of the process' output sends the text from the
    end of process to point.
`RET' before the end of the process' output copies the sexp ending at point
    to the end of the process' output, and sends it.
`DEL' converts tabs to spaces as it moves back.
`TAB' completes the input at point. IRB, Pry and Bond completion is supported.
`C-M-q' does `TAB' on each line starting within following expression.
Paragraphs are separated only by blank lines.  # start comments.
If you accidentally suspend your process, use \\[comint-continue-subjob]
to continue it.

The following commands are available:

\\{inf-ruby-mode-map}"
  (setq comint-prompt-regexp inf-ruby-prompt-pattern)
  (ruby-mode-variables)
  (when (bound-and-true-p ruby-use-smie)
    (set (make-local-variable 'smie-forward-token-function)
         #'inf-ruby-smie--forward-token)
    (set (make-local-variable 'smie-backward-token-function)
         #'inf-ruby-smie--backward-token))
  (add-hook 'comint-output-filter-functions 'inf-ruby-output-filter nil t)
  (setq comint-get-old-input 'inf-ruby-get-old-input)
  (set (make-local-variable 'compilation-error-regexp-alist)
       inf-ruby-error-regexp-alist)
  (set (make-local-variable 'comint-prompt-read-only) inf-ruby-prompt-read-only)
  (when (eq system-type 'windows-nt)
    (setq comint-process-echoes t))
  (add-hook 'completion-at-point-functions 'inf-ruby-completion-at-point nil t)
  (compilation-shell-minor-mode t))

(defun inf-ruby-output-filter (output)
  "Check if the current prompt is a top-level prompt."
  (unless (zerop (length output))
    (setq inf-ruby-last-prompt (car (last (split-string output "\n")))
          inf-ruby-at-top-level-prompt-p
          (string-match inf-ruby-first-prompt-pattern
                        inf-ruby-last-prompt))))

;; adapted from replace-in-string in XEmacs (subr.el)
(defun inf-ruby-remove-in-string (str regexp)
  "Remove all matches in STR for REGEXP and returns the new string."
  (let ((rtn-str "") (start 0) match prev-start)
    (while (setq match (string-match regexp str start))
      (setq prev-start start
            start (match-end 0)
            rtn-str (concat rtn-str (substring str prev-start match))))
    (concat rtn-str (substring str start))))

(defun inf-ruby-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (re-search-backward inf-ruby-first-prompt-pattern)
      (inf-ruby-remove-in-string (buffer-substring (point) end)
                                 inf-ruby-prompt-pattern))))

(defun inf-ruby-buffer ()
  "Return inf-ruby buffer for the current buffer or project."
  (let ((current-dir (locate-dominating-file default-directory
                                             #'inf-ruby-console-match)))
    (and current-dir
         (inf-ruby-buffer-in-directory current-dir))))

(defun inf-ruby-buffer-in-directory (dir)
  (setq dir (expand-file-name dir))
  (catch 'buffer
    (dolist (buffer inf-ruby-buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (string= (expand-file-name default-directory) dir)
            (throw 'buffer buffer)))))))

;;;###autoload
(defun inf-ruby (&optional impl)
  "Run an inferior Ruby process in a buffer.
With prefix argument, prompts for which Ruby implementation
\(from the list `inf-ruby-implementations') to use.

If there is a Ruby process running in an existing buffer, switch
to that buffer. Otherwise create a new buffer."
  (interactive (list (if current-prefix-arg
                         (completing-read "Ruby Implementation: "
                                          (mapc #'car inf-ruby-implementations))
                       inf-ruby-default-implementation)))
  (setq impl (or impl "ruby"))

  (let ((command (cdr (assoc impl inf-ruby-implementations))))
    (run-ruby-or-pop-to-buffer command impl
                               (or (inf-ruby-buffer)
                                   inf-ruby-buffer))))

;;;###autoload
(defun run-ruby (command &optional name)
  "Run an inferior Ruby process, input and output in a new buffer.

The consecutive buffer names will be:
`*NAME*', `*NAME*<2>', `*NAME*<3>' and so on.

NAME defaults to \"ruby\".

Runs the hooks `comint-mode-hook' and `inf-ruby-mode-hook'.

\(Type \\[describe-mode] in the process buffer for the list of commands.)"
  (setq name (or name "ruby"))

  (let ((commandlist (split-string-and-unquote command))
        (buffer (current-buffer))
        (process-environment process-environment))
    ;; http://debbugs.gnu.org/15775
    (setenv "PAGER" (executable-find "cat"))
    (set-buffer (apply 'make-comint-in-buffer
                       name
                       (generate-new-buffer-name (format "*%s*" name))
                       (car commandlist)
                       nil (cdr commandlist)))
    (inf-ruby-mode)
    (ruby-remember-ruby-buffer buffer)
    (push (current-buffer) inf-ruby-buffers)
    (setq inf-ruby-buffer-impl-name name
          inf-ruby-buffer-command command))

  (unless (and inf-ruby-buffer (comint-check-proc inf-ruby-buffer))
    (setq inf-ruby-buffer (current-buffer)))

  (pop-to-buffer (current-buffer)))

(defun run-ruby-or-pop-to-buffer (command &optional name buffer)
  (if (not (and buffer
                (comint-check-proc buffer)))
      (run-ruby command name)
    (pop-to-buffer buffer)
    (unless (and (string= inf-ruby-buffer-impl-name name)
                 (string= inf-ruby-buffer-command command))
      (error (concat "Found inf-ruby buffer, but it was created using "
                     "a different NAME-COMMAND combination: %s, `%s'")
             inf-ruby-buffer-impl-name
             inf-ruby-buffer-command))))

(defun inf-ruby-proc ()
  "Return the inferior Ruby process for the current buffer or project.

See variable `inf-ruby-buffers'."
  (or (get-buffer-process (if (eq major-mode 'inf-ruby-mode)
                              (current-buffer)
                            (or (inf-ruby-buffer)
                                inf-ruby-buffer)))
      (error "No current process. See variable inf-ruby-buffers")))

;; These commands are added to the inf-ruby-minor-mode keymap:

(defconst ruby-send-terminator "--inf-ruby-%x-%d-%d-%d--"
  "Template for irb here document terminator.
Must not contain ruby meta characters.")

(defconst inf-ruby-eval-binding
  (concat "(defined?(IRB) && IRB.conf[:MAIN_CONTEXT] && IRB.conf[:MAIN_CONTEXT].workspace.binding) || "
          "(defined?(Pry) && Pry.toplevel_binding)"))

(defconst ruby-eval-separator "")

(defun ruby-send-region (start end &optional print)
  "Send the current region to the inferior Ruby process."
  (interactive "r\nP")
  (let (term (file (or buffer-file-name (buffer-name))) line)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char start)
        (setq line (+ start (forward-line (- start)) 1))
        (goto-char start)
        (while (progn
                 (setq term (apply 'format ruby-send-terminator (random) (current-time)))
                 (re-search-forward (concat "^" (regexp-quote term) "$") end t)))))
    ;; compilation-parse-errors parses from second line.
    (save-excursion
      (let ((m (process-mark (inf-ruby-proc))))
        (set-buffer (marker-buffer m))
        (goto-char m)
        (insert ruby-eval-separator "\n")
        (set-marker m (point))))
    (comint-send-string (inf-ruby-proc) (format "eval <<'%s', %s, %S, %d\n"
                                                term inf-ruby-eval-binding
                                                file line))
    (comint-send-region (inf-ruby-proc) start end)
    (comint-send-string (inf-ruby-proc) (concat "\n" term "\n"))
    (when print (ruby-print-result))))

(defun ruby-print-result ()
  "Print the result of the last evaluation in the current buffer."
  (let ((proc (inf-ruby-proc)))
    (insert
     (with-current-buffer (inf-ruby-buffer)
       (while (not (and comint-last-prompt
                        (goto-char (car comint-last-prompt))
                        (looking-at inf-ruby-first-prompt-pattern)))
         (accept-process-output proc))
       (re-search-backward inf-ruby-prompt-pattern)
       (or (re-search-forward " => " (car comint-last-prompt) t)
           ;; Evaluation seems to have failed.
           ;; Try to extract the error string.
           (let* ((inhibit-field-text-motion t)
                  (s (buffer-substring-no-properties (point) (line-end-position))))
             (while (string-match inf-ruby-prompt-pattern s)
               (setq s (replace-match "" t t s)))
             (error "%s" s)))
       (buffer-substring-no-properties (point) (line-end-position))))))

(defun ruby-send-definition ()
  "Send the current definition to the inferior Ruby process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (ruby-beginning-of-defun)
      (ruby-send-region (point) end))))

(defun ruby-send-last-sexp (&optional print)
  "Send the previous sexp to the inferior Ruby process."
  (interactive "P")
  (ruby-send-region (save-excursion (ruby-backward-sexp) (point)) (point))
  (when print (ruby-print-result)))

(defun ruby-send-block (&optional print)
  "Send the current block to the inferior Ruby process."
  (interactive "P")
  (save-excursion
    (ruby-end-of-block)
    (end-of-line)
    (let ((end (point)))
      (ruby-beginning-of-block)
      (ruby-send-region (point) end)))
  (when print (ruby-print-result)))

(defvar ruby-last-ruby-buffer nil
  "The last buffer we switched to `inf-ruby' from.")
(make-variable-buffer-local 'ruby-last-ruby-buffer)

(defun ruby-remember-ruby-buffer (buffer)
  (setq ruby-last-ruby-buffer buffer))

(defun ruby-switch-to-inf (eob-p)
  "Switch to the ruby process buffer.
With argument, positions cursor at end of buffer."
  (interactive "P")
  (let ((buffer (current-buffer))
        (inf-ruby-buffer* (or (inf-ruby-buffer) inf-ruby-buffer)))
    (if inf-ruby-buffer*
        (progn
          (pop-to-buffer inf-ruby-buffer*)
          (ruby-remember-ruby-buffer buffer))
      (error "No current process buffer, see variable inf-ruby-buffers")))
  (cond (eob-p
         (push-mark)
         (goto-char (point-max)))))

(defun ruby-switch-to-last-ruby-buffer ()
  "Switch back to the last Ruby buffer."
  (interactive)
  (if (and ruby-last-ruby-buffer
           (buffer-live-p ruby-last-ruby-buffer))
      (pop-to-buffer ruby-last-ruby-buffer)
    (message "Don't know the original Ruby buffer")))

(defun ruby-send-region-and-go (start end)
  "Send the current region to the inferior Ruby process.
Then switch to the process buffer."
  (interactive "r")
  (ruby-send-region start end)
  (ruby-switch-to-inf t))

(defun ruby-send-definition-and-go ()
  "Send the current definition to the inferior Ruby.
Then switch to the process buffer."
  (interactive)
  (ruby-send-definition)
  (ruby-switch-to-inf t))

(defun ruby-send-block-and-go ()
  "Send the current block to the inferior Ruby.
Then switch to the process buffer."
  (interactive)
  (ruby-send-block)
  (ruby-switch-to-inf t))

(defun ruby-load-file (file-name)
  "Load a Ruby file into the inferior Ruby process."
  (interactive (comint-get-source "Load Ruby file: " ruby-prev-l/c-dir/file
                                  ruby-source-modes t)) ;; T because LOAD needs an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq ruby-prev-l/c-dir/file (cons (file-name-directory    file-name)
                                     (file-name-nondirectory file-name)))
  (comint-send-string (inf-ruby-proc) (concat "(load \""
                                              file-name
                                              "\"\)\n")))

(defun ruby-send-buffer ()
  "Send the current buffer to the inferior Ruby process."
  (interactive)
  (save-restriction
    (widen)
    (ruby-send-region (point-min) (point-max))))

(defun ruby-send-line ()
  "Send the current line to the inferior Ruby process."
  (interactive)
  (save-restriction
    (widen)
    (ruby-send-region (point-at-bol) (point-at-eol))))

(defun ruby-escape-single-quoted (str)
  "Escape single quotes, double quotes and newlines in STR."
  (replace-regexp-in-string "'" "\\\\'"
    (replace-regexp-in-string "\n" "\\\\n"
      (replace-regexp-in-string "\\\\" "\\\\\\\\" str))))

(defun inf-ruby-completions (expr)
  "Return a list of completions for the Ruby expression starting with EXPR."
  (let* ((proc (inf-ruby-proc))
         (line (buffer-substring (save-excursion (move-beginning-of-line 1)
                                                 (point))
                                 (point)))
         (comint-filt (process-filter proc))
         (kept "") completions
         ;; Guard against running completions in parallel:
         inf-ruby-at-top-level-prompt-p)
    (unless (equal "(rdb:1) " inf-ruby-last-prompt)
      (set-process-filter proc (lambda (proc string) (setq kept (concat kept string))))
      (unwind-protect
          (let ((completion-snippet
                 (format
                  (concat
                   "proc { |expr, line|"
                   "  require 'ostruct';"
                   "  old_wp = defined?(Bond) && Bond.started? && Bond.agent.weapon;"
                   "  begin"
                   "    Bond.agent.instance_variable_set('@weapon',"
                   "      OpenStruct.new(:line_buffer => line)) if old_wp;"
                   "    if defined?(_pry_.complete) then"
                   "      puts _pry_.complete(expr)"
                   "    else"
                   "      completer = if defined?(_pry_) then"
                   "        Pry.config.completer.build_completion_proc(binding, _pry_)"
                   "      elsif old_wp then"
                   "        Bond.agent"
                   "      elsif defined?(IRB::InputCompletor::CompletionProc) then"
                   "        IRB::InputCompletor::CompletionProc"
                   "      end and puts completer.call(expr).compact"
                   "    end"
                   "  ensure"
                   "    Bond.agent.instance_variable_set('@weapon', old_wp) if old_wp "
                   "  end "
                   "}.call('%s', '%s')\n")
                  (ruby-escape-single-quoted expr)
                  (ruby-escape-single-quoted line))))
            (process-send-string proc completion-snippet)
            (while (and (not (string-match inf-ruby-prompt-pattern kept))
                        (accept-process-output proc 2)))
            (setq completions (butlast (split-string kept "\r?\n") 2))
            ;; Subprocess echoes output on Windows and OS X.
            (when (and completions (string= (concat (car completions) "\n") completion-snippet))
              (setq completions (cdr completions))))
        (set-process-filter proc comint-filt)))
    completions))

(defconst inf-ruby-ruby-expr-break-chars " \t\n\"\'`><,;|&{(")

(defun inf-ruby-completion-bounds-of-expr-at-point ()
  "Return bounds of expression at point to complete."
  (when (not (memq (char-syntax (following-char)) '(?w ?_)))
    (save-excursion
      (let ((end (point)))
        (skip-chars-backward (concat "^" inf-ruby-ruby-expr-break-chars))
        (cons (point) end)))))

(defun inf-ruby-completion-expr-at-point ()
  "Return expression at point to complete."
  (let ((bounds (inf-ruby-completion-bounds-of-expr-at-point)))
    (and bounds
         (buffer-substring (car bounds) (cdr bounds)))))

(defun inf-ruby-completion-at-point ()
  "Retrieve the list of completions and prompt the user.
Returns the selected completion or nil."
  (let ((bounds (inf-ruby-completion-bounds-of-expr-at-point)))
    (when bounds
      (list (car bounds) (cdr bounds)
            (when inf-ruby-at-top-level-prompt-p
              (if (fboundp 'completion-table-with-cache)
                  (completion-table-with-cache #'inf-ruby-completions)
                (completion-table-dynamic #'inf-ruby-completions)))))))

(defvar inf-ruby-orig-compilation-mode nil
  "Original compilation mode before switching to `inf-ruby-mode'.")

(defvar inf-ruby-orig-process-filter nil
  "Original process filter before switching to `inf-ruby-mode'.")

(defun inf-ruby-switch-from-compilation ()
  "Make the buffer writable and switch to `inf-ruby-mode'.
Recommended for use when the program being executed enters
interactive mode, i.e. hits a debugger breakpoint."
  (interactive)
  (setq buffer-read-only nil)
  (buffer-enable-undo)
  (let ((mode major-mode)
        (arguments compilation-arguments)
        (orig-mode-line-process mode-line-process))
    (inf-ruby-mode)
    (make-local-variable 'inf-ruby-orig-compilation-mode)
    (setq inf-ruby-orig-compilation-mode mode)
    (set (make-local-variable 'compilation-arguments) arguments)
    (when orig-mode-line-process
      (setq mode-line-process orig-mode-line-process)))
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (make-local-variable 'inf-ruby-orig-process-filter)
      (setq inf-ruby-orig-process-filter (process-filter proc))
      (set-process-filter proc 'comint-output-filter))
    (when (looking-back inf-ruby-prompt-pattern (line-beginning-position))
      (let ((line (match-string 0)))
        (delete-region (match-beginning 0) (point))
        (comint-output-filter proc line)))))

(defun inf-ruby-maybe-switch-to-compilation ()
  "Switch to compilation mode this buffer was in before
`inf-ruby-switch-from-compilation' was called, if it was.
Otherwise, just toggle read-only status."
  (interactive)
  (if inf-ruby-orig-compilation-mode
      (let ((orig-mode-line-process mode-line-process)
            (proc (get-buffer-process (current-buffer)))
            (arguments compilation-arguments)
            (filter inf-ruby-orig-process-filter))
        (funcall inf-ruby-orig-compilation-mode)
        (setq mode-line-process orig-mode-line-process)
        (set (make-local-variable 'compilation-arguments) arguments)
        (when proc
          (set-process-filter proc filter)))
    (toggle-read-only)))

;;;###autoload
(defun inf-ruby-switch-setup ()
  "Modify `rspec-compilation-mode' and `ruby-compilation-mode'
keymaps to bind `inf-ruby-switch-from-compilation' to `С-x C-q'."
  (eval-after-load 'rspec-mode
    '(define-key rspec-compilation-mode-map (kbd "C-x C-q")
       'inf-ruby-switch-from-compilation))
  (eval-after-load 'ruby-compilation
    '(define-key ruby-compilation-mode-map (kbd "C-x C-q")
       'inf-ruby-switch-from-compilation))
  (eval-after-load 'projectile-rails
    '(define-key projectile-rails-server-mode-map (kbd "C-x C-q")
       'inf-ruby-switch-from-compilation)))

(defvar inf-ruby-console-patterns-alist
  '((".zeus.sock" . zeus)
    (inf-ruby-console-rails-p . rails)
    (inf-ruby-console-hanami-p . hanami)
    (inf-ruby-console-script-p . script)
    ("*.gemspec" . gem)
    (inf-ruby-console-racksh-p . racksh)
    ("Gemfile" . default))
  "Mapping from predicates (wildcard patterns or functions) to type symbols.
`inf-ruby-console-auto' walks up from the current directory until
one of the predicates matches, then calls `inf-ruby-console-TYPE',
passing it the found directory.")

(defvar inf-ruby-breakpoint-pattern "\\(\\[1\\] pry(\\)\\|\\((rdb:1)\\)\\|\\((byebug)\\)"
  "Pattern found when a breakpoint is triggered in a compilation session.
This checks if the current line is a pry or ruby-debug prompt.")

(defun inf-ruby-console-match (dir)
  "Find matching console command for DIR, if any."
  (catch 'type
    (dolist (pair inf-ruby-console-patterns-alist)
      (let ((default-directory dir)
            (pred (car pair)))
        (when (if (stringp pred)
                  (file-expand-wildcards pred)
                (funcall pred))
          (throw 'type (cdr pair)))))))

;;;###autoload
(defun inf-ruby-console-auto ()
  "Run the appropriate Ruby console command.
The command and the directory to run it from are detected
automatically."
  (interactive)
  (let* ((dir (locate-dominating-file default-directory
                                      #'inf-ruby-console-match))
         (type (inf-ruby-console-match dir))
         (fun (intern (format "inf-ruby-console-%s" type))))
    (unless type (error "No matching directory found"))
    (funcall fun dir)))

(defun inf-ruby-console-rails-p ()
  (and (file-exists-p "Gemfile.lock")
       (inf-ruby-file-contents-match "Gemfile.lock" "^ +railties ")
       (file-exists-p "config/application.rb")
       (inf-ruby-file-contents-match "config/application.rb"
                                     "\\_<Rails::Application\\_>")))

(defun inf-ruby-console-read-directory (type)
  (or
   (let ((predicate (car (rassq type inf-ruby-console-patterns-alist))))
     (locate-dominating-file (read-directory-name "" nil nil t)
                             (lambda (dir)
                               (let ((default-directory dir))
                                 (if (stringp predicate)
                                     (file-expand-wildcards predicate)
                                   (funcall predicate))))))
   (error "No matching directory for %s console found"
          (capitalize (symbol-name type)))))

(defun inf-ruby-console-run (command name)
  (run-ruby-or-pop-to-buffer command name
                             (inf-ruby-buffer-in-directory default-directory)))

;;;###autoload
(defun inf-ruby-console-zeus (dir)
  "Run Rails console in DIR using Zeus."
  (interactive (list (inf-ruby-console-read-directory 'zeus)))
  (let ((default-directory (file-name-as-directory dir))
        (exec-prefix (if (executable-find "zeus") "" "bundle exec ")))
    (inf-ruby-console-run (concat exec-prefix "zeus console") "zeus")))

;;;###autoload
(defun inf-ruby-console-rails (dir)
  "Run Rails console in DIR."
  (interactive (list (inf-ruby-console-read-directory 'rails)))
  (let* ((default-directory (file-name-as-directory dir))
         (env (inf-ruby-console-rails-env))
         (with-bundler (file-exists-p "Gemfile")))
    (inf-ruby-console-run
     (concat (when with-bundler "bundle exec ")
             "rails console "
             env)
     "rails")))

(defun inf-ruby-console-rails-env ()
  (if (stringp inf-ruby-console-environment)
      inf-ruby-console-environment
    (let ((envs (inf-ruby-console-rails-envs)))
      (completing-read "Rails environment: "
                       envs
                       nil t
                       nil nil (car (member "development" envs))))))

(defun inf-ruby-console-rails-envs ()
  (let ((files (file-expand-wildcards "config/environments/*.rb")))
    (if (null files)
        (error "No files in %s" (expand-file-name "config/environments/"))
      (mapcar #'file-name-base files))))

(defun inf-ruby-console-hanami-p ()
  (and (file-exists-p "config.ru")
       (inf-ruby-file-contents-match "config.ru" "\\_<run Hanami.app\\_>")))

(defun inf-ruby-console-hanami (dir)
  "Run Hanami console in DIR."
  (interactive (list (inf-ruby-console-read-directory 'hanami)))
  (let* ((default-directory (file-name-as-directory dir))
         (env (inf-ruby-console-hanami-env))
         (with-bundler (file-exists-p "Gemfile"))
         (process-environment (cons (format "HANAMI_ENV=%s" env)
                                    process-environment)))
    (inf-ruby-console-run
     (concat (when with-bundler "bundle exec ")
             "hanami console")
     "hanami")))

(defun inf-ruby-console-hanami-env ()
  (if (stringp inf-ruby-console-environment)
      inf-ruby-console-environment
    (let ((envs '("development" "test" "production")))
      (completing-read "Hanami environment: "
                       envs
                       nil t
                       nil nil (car (member "development" envs))))))

;;;###autoload
(defun inf-ruby-console-gem (dir)
  "Run IRB console for the gem in DIR.
The main module should be loaded automatically.  If DIR contains a
Gemfile, it should use the `gemspec' instruction."
  (interactive (list (inf-ruby-console-read-directory 'gem)))
  (let* ((default-directory (file-name-as-directory dir))
         (gemspec (car (file-expand-wildcards "*.gemspec")))
         (base-command
          (if (file-exists-p "Gemfile")
              (if (inf-ruby-file-contents-match gemspec "\\$LOAD_PATH")
                  "bundle exec irb"
                "bundle exec irb -I lib")
            "irb -I lib"))
         (name (inf-ruby-file-contents-match
                gemspec "\\.name[ \t]*=[ \t]*['\"]\\([^'\"]+\\)['\"]" 1))
         args files)
    (unless (file-exists-p "lib")
      (error "The directory must contain a 'lib' subdirectory"))
    (let ((feature (and name (replace-regexp-in-string "-" "/" name))))
      (if (and feature (file-exists-p (concat "lib/" feature ".rb")))
          ;; There exists the main file corresponding to the gem name,
          ;; let's require it.
          (setq args (concat " -r " feature))
        ;; Let's require all non-directory files under lib, instead.
        (dolist (item (directory-files "lib"))
          (when (and (not (file-directory-p (format "lib/%s" item)))
                     (string-match-p "\\.rb\\'" item))
            (push item files)))
        (setq args
              (mapconcat
               (lambda (file)
                 (concat " -r " (file-name-sans-extension file)))
               files
               ""))))
    (inf-ruby-console-run (concat base-command args
                                  " -r irb/completion")
                          "gem")))

(defun inf-ruby-console-racksh-p ()
  (and (file-exists-p "Gemfile.lock")
       (inf-ruby-file-contents-match "Gemfile.lock" "^ +racksh ")))

(defun inf-ruby-console-racksh (dir)
  "Run racksh in DIR."
  (interactive (list (inf-ruby-console-read-directory 'racksh)))
  (let ((default-directory (file-name-as-directory dir)))
    (inf-ruby-console-run "bundle exec racksh" "racksh")))

(defun inf-ruby-in-ruby-compilation-modes (mode)
  "Check if MODE is a Ruby compilation mode."
  (member mode '(rspec-compilation-mode
                 ruby-compilation-mode
                 projectile-rails-server-mode)))

;;;###autoload
(defun inf-ruby-auto-enter ()
  "Switch to `inf-ruby-mode' if the breakpoint pattern matches the current line."
  (when (and (inf-ruby-in-ruby-compilation-modes major-mode)
             (save-excursion
               (beginning-of-line)
               (re-search-forward inf-ruby-breakpoint-pattern nil t)))
    ;; Exiting excursion before this call to get the prompt fontified.
    (inf-ruby-switch-from-compilation)
    (add-hook 'comint-input-filter-functions 'inf-ruby-auto-exit nil t)))

;;;###autoload
(defun inf-ruby-auto-exit (input)
  "Return to the previous compilation mode if INPUT is a debugger exit command."
  (when (inf-ruby-in-ruby-compilation-modes inf-ruby-orig-compilation-mode)
    (if (member input '("quit\n" "exit\n" ""))
        ;; After the current command completes, otherwise we get a
        ;; marker error.
        (run-with-idle-timer 0 nil #'inf-ruby-maybe-switch-to-compilation))))

(defun inf-ruby-enable-auto-breakpoint ()
  (interactive)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter))

(defun inf-ruby-disable-auto-breakpoint ()
  (interactive)
  (remove-hook 'compilation-filter-hook 'inf-ruby-auto-enter))

(defun inf-ruby-console-script-p ()
  (and (file-exists-p "Gemfile.lock")
       (or
        (file-exists-p "bin/console")
        (file-exists-p "console")
        (file-exists-p "console.rb"))))

;;;###autoload
(defun inf-ruby-console-script (dir)
  "Run custom bin/console, console or console.rb in DIR."
  (interactive (list (inf-ruby-console-read-directory 'script)))
  (let ((default-directory (file-name-as-directory dir)))
    (cond
     ((file-exists-p "bin/console")
      (inf-ruby-console-run "bundle exec bin/console" "bin/console"))
     ((file-exists-p "console.rb")
      (inf-ruby-console-run "bundle exec ruby console.rb" "console.rb"))
     ((file-exists-p "console")
      (inf-ruby-console-run "bundle exec console" "console.rb")))))

;;;###autoload
(defun inf-ruby-console-default (dir)
  "Run Pry, or bundle console, in DIR."
  (interactive (list (inf-ruby-console-read-directory 'default)))
  (let ((default-directory (file-name-as-directory dir)))
    (unless (file-exists-p "Gemfile")
      (error "The directory must contain a Gemfile"))
    (cond
     ((inf-ruby-file-contents-match "Gemfile" "[\"']pry[\"']")
      (inf-ruby-console-run "bundle exec pry" "pry"))
     (t
      (inf-ruby-console-run "bundle console" "bundle console")))))

;;;###autoload
(defun inf-ruby-file-contents-match (file regexp &optional match-group)
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward regexp nil t)
      (if match-group
          (match-string match-group)
        t))))

(defun inf-ruby-smie--forward-token ()
  (let ((inhibit-field-text-motion t))
    (ruby-smie--forward-token)))

(defun inf-ruby-smie--backward-token ()
  (let ((inhibit-field-text-motion t))
    (ruby-smie--backward-token)))

;;;###autoload (dolist (mode ruby-source-modes) (add-hook (intern (format "%s-hook" mode)) 'inf-ruby-minor-mode))

(provide 'inf-ruby)
;;; inf-ruby.el ends here
