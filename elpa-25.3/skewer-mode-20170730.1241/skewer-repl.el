;;; skewer-repl.el --- create a REPL in a visiting browser -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This is largely based on of IELM's code. Run `skewer-repl' to
;; switch to the REPL buffer and evaluate code. Use
;; `skewer-repl-toggle-strict-mode' to turn strict mode on and off.

;; If `compilation-search-path' is set up properly, along with
;; `skewer-path-strip-level', asynchronous errors will provide
;; clickable error messages that will take you to the source file of
;; the error. This is done using `compilation-shell-minor-mode'.

;;; Code:

(require 'comint)
(require 'compile)
(require 'skewer-mode)

(defcustom skewer-repl-strict-p nil
  "When non-NIL, all REPL evaluations are done in strict mode."
  :type 'boolean
  :group 'skewer)

(defcustom skewer-repl-prompt "js> "
  "Prompt string for JavaScript REPL."
  :type 'string
  :group 'skewer)

(defvar skewer-repl-welcome
  (propertize "*** Welcome to Skewer ***\n"
              'font-lock-face 'font-lock-comment-face)
  "Header line to show at the top of the REPL buffer. Hack
notice: this allows log messages to appear before anything is
evaluated because it provides insertable space at the top of the
buffer.")

(defun skewer-repl-process ()
  "Return the process for the skewer REPL."
  (get-buffer-process (current-buffer)))

(defface skewer-repl-log-face
  '((((class color) (background light))
     :foreground "#77F")
    (((class color) (background dark))
     :foreground "#77F"))
  "Face for skewer.log() messages."
  :group 'skewer)

(define-derived-mode skewer-repl-mode comint-mode "js-REPL"
  "Provide a REPL into the visiting browser."
  :group 'skewer
  :syntax-table emacs-lisp-mode-syntax-table
  (setq comint-prompt-regexp (concat "^" (regexp-quote skewer-repl-prompt))
        comint-input-sender 'skewer-input-sender
        comint-process-echoes nil)
  ;; Make opportunistic use of company-mode, but don't require it.
  ;; This means company-backends may be undeclared, so don't emit a
  ;; warning about it.
  (with-no-warnings
    (setq-local company-backends '(company-skewer-repl)))
  (unless (comint-check-proc (current-buffer))
    (insert skewer-repl-welcome)
    (start-process "skewer-repl" (current-buffer) nil)
    (set-process-query-on-exit-flag (skewer-repl-process) nil)
    (goto-char (point-max))
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)
    (comint-output-filter (skewer-repl-process) skewer-repl-prompt)
    (set-process-filter (skewer-repl-process) 'comint-output-filter)))

(defun skewer-repl-toggle-strict-mode ()
  "Toggle strict mode for expressions evaluated by the REPL."
  (interactive)
  (setq skewer-repl-strict-p (not skewer-repl-strict-p))
  (message "REPL strict mode %s"
           (if skewer-repl-strict-p "enabled" "disabled")))

(defun skewer-input-sender (_ input)
  "REPL comint handler."
  (skewer-eval input 'skewer-post-repl
               :verbose t :strict skewer-repl-strict-p))

(defun skewer-post-repl (result)
  "Callback for reporting results in the REPL."
  (let ((buffer (get-buffer "*skewer-repl*"))
        (output (cdr (assoc 'value result))))
    (when buffer
      (with-current-buffer buffer
        (comint-output-filter (skewer-repl-process)
                              (concat output "\n" skewer-repl-prompt))))))

(defvar skewer-repl-types
  '(("log" . skewer-repl-log-face)
    ("error" . skewer-error-face))
  "Faces to use for different types of log messages.")

(defun skewer-log-filename (log)
  "Create a log string for the source file in LOG if present."
  (let ((name (cdr (assoc 'filename log)))
        (line (cdr (assoc 'line log)))
        (column (cdr (assoc 'column log))))
    (when name
      (concat (format "\n    at %s:%s" name line)
              (if column (format ":%s" column))))))

(defun skewer-post-log (log)
  "Callback for logging messages to the REPL."
  (let* ((buffer (get-buffer "*skewer-repl*"))
         (face (cdr (assoc (cdr (assoc 'type log)) skewer-repl-types)))
         (value (or (cdr (assoc 'value log)) "<unspecified error>"))
         (output (propertize value 'font-lock-face face)))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-max))
          (forward-line 0)
          (backward-char)
          (insert (concat "\n" output (skewer-log-filename log))))))))

(defcustom skewer-path-strip-level 1
  "Number of folders which will be stripped from url when discovering paths.
Use this to limit path matching to files in your filesystem. You
may want to add some folders to `compilation-search-path', so
matched files can be found."
  :type 'number
  :group 'skewer)

(defun skewer-repl-mode-compilation-shell-hook ()
  "Setup compilation shell minor mode for highlighting files"
  (let ((error-re (format "^[ ]*at https?://[^/]+/\\(?:[^/]+/\\)\\{%d\\}\\([^:?#]+\\)\\(?:[?#][^:]*\\)?:\\([[:digit:]]+\\)\\(?::\\([[:digit:]]+\\)\\)?$" skewer-path-strip-level)))
    (setq-local compilation-error-regexp-alist `((,error-re 1 2 3 2))))
  (compilation-shell-minor-mode 1))

;;;###autoload
(defun skewer-repl--response-hook (response)
  "Catches all browser messages logging some to the REPL."
  (let ((type (cdr (assoc 'type response))))
    (when (member type '("log" "error"))
      (skewer-post-log response))))

;;;###autoload
(defun skewer-repl ()
  "Start a JavaScript REPL to be evaluated in the visiting browser."
  (interactive)
  (when (not (get-buffer "*skewer-repl*"))
    (with-current-buffer (get-buffer-create "*skewer-repl*")
      (skewer-repl-mode)))
  (pop-to-buffer (get-buffer "*skewer-repl*")))

(defun company-skewer-repl (command &optional arg &rest _args)
  "Skewerl REPL backend for company-mode.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (cl-case command
    (interactive
     (with-no-warnings ;; opportunistic use of company-mode
       (company-begin-backend 'company-skewer-repl)))
    (prefix (skewer-repl-company-prefix))
    (ignore-case t)
    (sorted t)
    (candidates (cons :async
                      (lambda (callback)
                        (skewer-repl-get-completions arg callback))))))

(defun skewer-repl-get-completions (arg callback)
  "Get the completion list matching the prefix ARG.
Evaluate CALLBACK with the completion candidates."
  (let* ((expression (skewer-repl--get-completion-expression arg))
         (pattern (if expression
                      (substring arg (1+ (length expression)))
                    arg)))
    (skewer-eval (or expression "window")
                 (lambda (result)
                   (cl-loop with value = (cdr (assoc 'value result))
                            for key being the elements of value
                            when expression
                            collect (concat expression "." key) into results
                            else
                            collect key into results
                            finally (funcall callback results)))
                 :type "completions"
                 :extra `((regexp . ,pattern)))))

(defun skewer-repl--get-completion-expression (arg)
  "Get completion expression from ARG."
  (let ((components (split-string arg "\\.")))
    (when (> (length components) 1)
      (mapconcat #'identity (cl-subseq components 0 -1) "."))))

(defun skewer-repl-company-prefix ()
  "Prefix for company."
  (and (eq major-mode 'skewer-repl-mode)
       (or (with-no-warnings ;; opportunistic use of company-mode
             (company-grab-symbol-cons "\\." 1))
           'stop)))

;;;###autoload
(eval-after-load 'skewer-mode
  '(progn
     (add-hook 'skewer-response-hook #'skewer-repl--response-hook)
     (add-hook 'skewer-repl-mode-hook #'skewer-repl-mode-compilation-shell-hook)
     (define-key skewer-mode-map (kbd "C-c C-z") #'skewer-repl)))

(provide 'skewer-repl)

;;; skewer-repl.el ends here
