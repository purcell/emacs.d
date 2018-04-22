;;; robe.el --- Code navigation, documentation lookup and completion for Ruby -*- lexical-binding: t -*-

;; Copyright © 2012 Phil Hagelberg
;; Copyright © 2012-2017 Dmitry Gutov

;; Author: Dmitry Gutov
;; URL: https://github.com/dgutov/robe
;; Version: 0.8.1
;; Keywords: ruby convenience rails
;; Package-Requires: ((inf-ruby "2.5.1") (emacs "24.4"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; You can jump to or read the documentation for the method, module (jump only),
;; `super` or constructor definition at point.
;;
;; ElDoc support and constant and method completion are also provided.

;;; Usage

;; (add-hook 'ruby-mode-hook 'robe-mode)
;;
;;  - M-. to jump to the definition
;;  - M-, to jump back
;;  - C-c C-d to see the documentation
;;  - C-c C-k to refresh Rails environment
;;  - C-M-i to complete the symbol at point

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'inf-ruby)
(require 'etags)
(require 'json)
(require 'url)
(require 'url-http)
(require 'url-handlers)
(require 'cl-lib)
(require 'thingatpt)
(require 'eldoc)
(require 'help-mode)
(require 'ruby-mode)

(defgroup robe nil
  "Code navigation, documentation lookup and completion for Ruby"
  :group 'languages
  :group 'convenience)

(defcustom robe-highlight-capf-candidates t
  "When non-nil, `completion-at-point' candidates buffer will
have constants, methods and arguments highlighted in color."
  :group 'robe
  :type 'boolean)

(defvar robe-ruby-path
  (let ((current (or load-file-name (buffer-file-name))))
    (expand-file-name "lib" (file-name-directory current)))
  "Path to the backend Ruby code.")

(defvar-local robe-port nil)

(defvar robe-jump-conservative nil)

(defvar-local robe-running nil)

(defvar-local robe-load-path nil)

(defcustom robe-completing-read-func 'completing-read-default
  "Function to call for completing reads, to resolve ambiguous names.

Will not be used when either `completing-read' or
`completing-read-function' are [temporarily] overridden by user
or another package."
  :type '(choice (const :tag "Ido" ido-completing-read)
                 (const :tag "Plain" completing-read-default)
                 (function :tag "Other function"))
  :group 'robe)

(defcustom robe-find-file-hook nil
  "Normal hook run after visiting a file."
  :type 'hook
  :group 'robe)

(defun robe-completing-read (&rest args)
  (let ((completing-read-function
         ; 1) allow read-function override. Subtle: an *old* customization
         ; value 'completing-read would cause infinite loop, so avoid.
         (if (and (eq completing-read-function 'completing-read-default)
                  (not (eq robe-completing-read-func 'completing-read)))
             robe-completing-read-func
           completing-read-function)))
    (apply #'completing-read args)))      ; 2) allow completing-read override

(defmacro robe-with-inf-buffer (&rest body)
  `(let ((buf (robe-inf-buffer)))
     (when buf
       (with-current-buffer buf
         ,@body))))

(defun robe-start (&optional force)
  "Start Robe server if it isn't already running.
When called with a prefix argument, kills the current Ruby
process, if any, and starts a new console for the current
project."
  (interactive "P")
  (let* ((ruby-buffer (robe-inf-buffer))
         (process (get-buffer-process ruby-buffer)))
    (when (or force (not process))
      (when (buffer-live-p ruby-buffer)
        (with-current-buffer ruby-buffer
          (setq robe-running nil)))
      (when process
        (delete-process process))
      (if (or force
              (yes-or-no-p "No Ruby console running. Launch automatically?"))
          (let ((conf (current-window-configuration)))
            (when (buffer-live-p ruby-buffer)
              (kill-buffer ruby-buffer))
            (inf-ruby-console-auto)
            (set-window-configuration conf))
        (error "Aborted"))))
  (when (not (robe-running-p))
    (let* ((proc (inf-ruby-proc))
           started failed
           (comint-filter (process-filter proc))
           (tmp-filter (lambda (p s)
                         (cond
                          ((string-match "robe on \\([0-9]+\\)" s)
                           (setq started t)
                           (with-current-buffer (process-buffer proc)
                             (setq robe-port (string-to-number
                                              (match-string 1 s)))))
                          ((let (case-fold-search)
                             (string-match-p "Error\\>" s))
                           (setq failed t)))
                         (funcall comint-filter p s)))
           (script (format (mapconcat #'identity
                                      '("unless defined? Robe"
                                        "  $:.unshift '%s'"
                                        "  require 'robe'"
                                        "end"
                                        "Robe.start\n")
                                      ";")
                           robe-ruby-path)))
      (unwind-protect
          (progn
            (set-process-filter proc tmp-filter)
            (comint-send-string proc script)
            (while (not started)
              (unless (process-live-p proc) (setq failed t))
              (when failed
                (ruby-switch-to-inf t)
                (error "Robe launch failed"))
              (accept-process-output proc))
            (set-process-sentinel proc #'robe-process-sentinel))
        (set-process-filter proc comint-filter)))
    (when (robe-request "ping") ;; Should be always t when no error, though.
      (robe-with-inf-buffer
       (setq robe-running t
             robe-load-path (mapcar #'file-name-as-directory
                                    (robe-request "load_path"))))
      (message "Robe connection established"))))

(defun robe-inf-buffer ()
  ;; Using locate-dominating-file in a large directory
  ;; (such as .rbenv//gems/), or file-in-directory-p for all LOAD_PATH
  ;; entries, turns out to be too slow.
  (let ((dd (expand-file-name default-directory))
        (inf-buffers (cl-remove-if-not
                      (lambda (buf)
                        (and (buffer-live-p buf)
                             (get-buffer-process buf)))
                      inf-ruby-buffers)))
    (cond
     ((null inf-buffers)
      nil)
     ((= 1 (length inf-buffers))
      (car inf-buffers))
     (t
      ;; More than one inf-ruby process, let's use the more complex
      ;; detection logic.
      (or
       (robe-find-inf-buffer
        (lambda ()
          (string-prefix-p (expand-file-name default-directory) dd))
        inf-buffers)
       (robe-find-inf-buffer
        (lambda ()
          (cl-find dd robe-load-path
                   :test (lambda (dd path-element)
                           (string-prefix-p path-element dd))))
        inf-buffers))))))

(defun robe-find-inf-buffer (predicate buffers)
  (catch 'buffer
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when (funcall predicate)
          (throw 'buffer buffer))))))

(defun robe-running-p ()
  (robe-with-inf-buffer
   robe-running))

(defun robe-process-sentinel (proc _event)
  (when (memq (process-status proc) '(signal exit))
    (setq robe-running nil)))

(defun robe-request (endpoint &rest args)
  (let* ((base-url (robe-with-inf-buffer
                    (format "http://127.0.0.1:%s/" robe-port)))
         (url (format "%s/%s/%s" base-url endpoint
                      (mapconcat (lambda (arg)
                                   (cond ((eq arg t) "yes")
                                         ((cl-plusp (length arg))
                                          (url-hexify-string arg))
                                         (t "-")))
                                 args "/")))
         (response-buffer (robe-retrieve url)))
    (message nil) ;; So "Contacting host" message is cleared
    (if response-buffer
        (prog1
            (with-temp-buffer
              (url-insert response-buffer)
              (goto-char (point-min))
              (let ((json-array-type 'list))
                (json-read)))
          (kill-buffer response-buffer))
      (error "Server doesn't respond"))))

(defun robe-retrieve (url)
  (defvar url-http-response-status)
  (let ((buffer (condition-case nil (url-retrieve-synchronously url t)
                  (file-error nil))))
    (if (and buffer
             (with-current-buffer buffer
               (memq url-http-response-status '(200 500))))
        buffer
      (robe-with-inf-buffer
       (setq robe-running nil)))))

(cl-defstruct (robe-spec (:type list)) module inst-p method params file line)

(defun robe-ask ()
  "Prompt for module, method, and jump to its definition."
  (interactive)
  (robe-jump-to (robe-ask-prompt)))

(defun robe-ask-prompt ()
  (let* ((modules (robe-request "modules"))
         (module (robe-completing-read "Module: " modules))
         (targets (robe-request "targets" module))
         (_ (unless targets (error "No methods found")))
         (alist (robe-decorate-methods (cdr targets))))
    (cdr (assoc (robe-completing-read "Method: " alist nil t)
                alist))))

(defun robe-decorate-methods (list)
  (mapcar (lambda (spec)
            (cons (concat (if (robe-spec-inst-p spec) "#" ".")
                          (robe-spec-method spec))
                  spec))
          list))

(defun robe-const-p (thing)
  (let (case-fold-search) (string-match "\\`\\([A-Z]\\|::\\)" thing)))

(defun robe-jump (arg)
  "Jump to the method or module at point, prompt for module or file if necessary.
If invoked with a prefix or no symbol at point, delegate to `robe-ask'."
  (interactive "P")
  (robe-start)
  (let ((thing (robe--jump-thing)))
    (cond
     ((or (not thing) arg)
      (robe-ask))
     ((robe-const-p thing)
      (robe-jump-to-module thing))
     (t
      (robe-jump-to (robe-jump-prompt thing))))))

(defun robe--jump-thing ()
  (let* ((bounds (robe-complete-bounds))
         (thing (buffer-substring (car bounds) (cdr bounds))))
    (if (and thing (save-excursion
                     (goto-char (cdr bounds))
                     (looking-at " *=[^=]")))
        (concat thing "=")
      thing)))

(defun robe-jump-prompt (thing)
  (let* ((alist (robe-decorate-modules (robe-jump-modules thing
                                                          (robe-call-context)))))
    (unless alist (error "Method not found"))
    (if (= 1 (length alist))
        (cdar alist)
      (cdr (assoc (robe-completing-read "Module: " alist nil t)
                  alist)))))

(defun robe-jump-modules (thing context)
  (cl-destructuring-bind (target module instance ctx) context
    (let (super)
      (unless target
        (when (string= thing "super")
          (setq thing (nth 2 ctx)
                super t)))
      (robe-request "method_targets"
                       thing target module instance super
                       robe-jump-conservative))))

(defun robe-call-context ()
  (let* ((target (save-excursion
                   (and (progn (ignore-errors (beginning-of-thing 'symbol))
                               (eq ?. (char-before)))
                        (progn (forward-char -1)
                               (skip-chars-backward " \n\r\t")
                               (or (thing-at-point 'symbol)
                                   "!")))))
         (ctx (robe-context))
         (module (car ctx))
         (_ (when (string= target "self") (setq target nil)))
         (instance (unless target (nth 1 ctx))))
    (list target module instance ctx)))

(defun robe-decorate-modules (list)
  (cl-loop for spec in list
        for name = (cond ((robe-spec-module spec))
                         ((robe-spec-file spec)
                          (format "<%s>" (file-name-nondirectory
                                          (robe-spec-file spec)))))
        when name
        collect (cons (concat name
                              (if (robe-spec-inst-p spec) "#" "."))
                      (cons name (cdr spec)))))

(defun robe-jump-to-module (name)
  "Prompt for module, jump to a file where it has method definitions."
  (interactive `(,(robe-completing-read "Module: " (robe-request "modules"))))
  (let ((paths (robe-request "class_locations" name (car (robe-context)))))
    (when (null paths) (error "Can't find the location"))
    (let ((file (if (= (length paths) 1)
                    (car paths)
                  (let ((alist (robe-to-abbr-paths paths)))
                    (cdr (assoc (robe-completing-read "File: " alist nil t)
                                alist))))))
      (robe-find-file file)
      (goto-char (point-min))
      (let* ((nesting (split-string name "::"))
             (cnt (1- (length nesting)))
             case-fold-search)
        (re-search-forward (concat "^[ \t]*\\(class\\|module\\) +.*\\_<"
                                   (cl-loop for i from 1 to cnt
                                            concat "\\(")
                                   (mapconcat #'identity nesting "::\\)?")
                                   "\\_>")))
      (back-to-indentation))))

(defun robe-to-abbr-paths (list)
  (let* ((sorted (sort (copy-sequence list) #'string-lessp))
         (first (car sorted))
         (last (car (last sorted)))
         (len (cl-loop for i from 0 to (min (length first)
                                         (length last))
                    when (/= (aref first i) (aref last i))
                    return i)))
    (unless (zerop len)
      (while (/= (aref first (1- len)) ?/) (cl-decf len)))
    (mapcar (lambda (path) (cons (substring path len) path)) list)))

(defun robe-context ()
  (let ((current-method (and add-log-current-defun-function
                             (funcall add-log-current-defun-function))))
    (if current-method
        ;; Side-stepping the module methods bug in the above function.
        (let* ((segments (split-string current-method "#\\|\\.\\|::" t))
               (method-name (when (string-match "\\.\\|#" current-method)
                              (car (last segments))))
               (instance (string-match "#" current-method))
               (module (mapconcat 'identity (if method-name
                                                (butlast segments)
                                              segments) "::")))
          (set-text-properties 0 (length module) nil module) ;; for debugging
          (set-text-properties 0 (length method-name) nil method-name)
          (list module (when instance t) method-name))
      (list nil t nil))))

(defun robe-jump-to (spec &optional pop-to-buffer)
  (let ((file (robe-spec-file spec)))
    (if (null file)
        (when (yes-or-no-p "Can't jump to a C method. Show documentation? ")
          (robe-show-doc spec))
      (robe-find-file file pop-to-buffer)
      (goto-char (point-min))
      (forward-line (1- (robe-spec-line spec)))
      (back-to-indentation))))

(defun robe-find-file (file &optional pop-to-buffer)
  (unless (file-exists-p file)
    (error "'%s' does not exist" file))
  (if pop-to-buffer
      (pop-to-buffer (find-file-noselect file))
    (ring-insert find-tag-marker-ring (point-marker))
    (find-file file))
  (run-hooks 'robe-find-file-hook))

(defun robe-rails-refresh ()
  "Pick up changes in the loaded classes and detect new files.
Only works with Rails, see e.g. `rinari-console'."
  (interactive)
  (robe-start)
  (robe-request "rails_refresh")
  (message "Done"))

(defun robe-doc (arg)
  "Show docstring for the method at point."
  (interactive "P")
  (robe-start)
  (let ((thing (robe--jump-thing)))
    (robe-show-doc (if (or (not thing) arg)
                          (robe-ask-prompt)
                        (robe-jump-prompt thing)))))

(defvar robe-code-face 'font-lock-preprocessor-face)

(defvar robe-em-face 'font-lock-variable-name-face)

(defvar robe-doc-rules
  '(("<\\(tt\\|code\\)>\\(.+?\\)</\\1>" robe-doc-hl-text 2 robe-code-face)
    ("\\_<\\+\\([^[:space:]]+\\)\\+\\_>" robe-doc-hl-text 1 robe-code-face)
    ("<\\(i\\|em\\)>\\(.+?\\)</\\1>" robe-doc-hl-text 2 robe-em-face)
    ("\\_<_\\([^_][^[:space:]]*\\)_\\_>" robe-doc-hl-text 1 robe-em-face)
    ("\\(``\\).*?\\(''\\)" robe-doc-replace-text (1 . "\u201c") (2 . "\u201d"))
    ("\\(`\\).*?\\('\\)" robe-doc-replace-text (1 . "\u2018") (2 . "\u2019"))))

(define-button-type 'robe-method-def
  :supertype 'help-xref
  'help-function #'robe-jump-to
  'help-echo "mouse-2, RET: find method's definition")

(define-button-type 'robe-toggle-source
  'action 'robe-toggle-source
  'help-echo "mouse-2, RET: toggle source")

(defun robe-show-doc (spec)
  (let* ((doc (robe-doc-for spec))
         (buffer (get-buffer-create "*robe-doc*"))
         (inhibit-read-only t)
         (docstring (cdr (assoc 'docstring doc)))
         (source (cdr (assoc 'source doc)))
         (aliases (cdr (assoc 'aliases doc)))
         (visibility (cdr (assoc 'visibility doc)))
         (file (robe-spec-file spec)))
    (with-help-window buffer
      (unless (zerop (length docstring))
        (princ "\n\n")
        (princ docstring)))
    (with-current-buffer buffer
      (robe-doc-fontify-regions)
      (when source
        (insert "\n")
        (let ((button (insert-text-button "Source"
                                          'type 'robe-toggle-source)))
          (insert "\n\n")
          (if file
              (let ((beg (point)))
                (insert source)
                (robe-doc-fontify-ruby beg (point)))
            (insert (robe-doc-fontify-c-string source)))
          (robe-toggle-source button)))
      (goto-char (point-min))
      (save-excursion
        (insert (robe-signature spec))
        (when file
          (insert " is defined in ")
          (insert-text-button (file-name-nondirectory file)
                              'type 'robe-method-def
                              'help-args (list spec t)))
        (when (equal visibility "public")
          (setq visibility nil))
        (when (or aliases visibility)
          (insert "\n"))
        (when aliases
          (insert "\nAliases: " (mapconcat #'identity aliases ", ")))
        (when visibility
          (insert "\nVisibility: " visibility)))
      (visual-line-mode 1))))

(defun robe-doc-fontify-regions ()
  (let (last-pos)
    (while (not (eobp))
      (when last-pos (robe-doc-apply-rules last-pos (point)))
      (while (looking-at "\\(\n\\)+\\( +.*\n\\)+\\(\n\\|\\'\\)")
        (save-match-data
          (robe-doc-fontify-ruby (match-beginning 0) (match-end 0)))
        (goto-char (match-end 2)))
      (setq last-pos (point))
      (re-search-forward "[^[:space:]]\n *$" nil 'move))
    (robe-doc-apply-rules last-pos (point))))

(defun robe-doc-apply-rules (from to)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?- "." table)
    (with-syntax-table table
      (save-excursion
        (goto-char from)
        (cl-loop for (re fn . args) in robe-doc-rules do
                 (save-excursion
                   (while (re-search-forward re to t)
                     (apply fn args))))))))

(defun robe-doc-hl-text (group face)
  (replace-match (format "\\%d" group))
  (put-text-property (match-beginning 0) (match-end 0)
                     'face (symbol-value face)))

(defun robe-doc-replace-text (&rest rules)
  (cl-loop for (group . replacement) in rules do
           (replace-match replacement t nil nil group)))

(defun robe-doc-fontify-ruby (from to)
  (let ((syntax-propertize-function #'ruby-syntax-propertize-function)
        (font-lock-defaults (list ruby-font-lock-keywords nil nil))
        (font-lock-syntax-table ruby-font-lock-syntax-table)
        (font-lock-dont-widen t))
    (save-restriction
      (narrow-to-region from to)
      (font-lock-fontify-region from to))))

(defun robe-doc-fontify-c-string (string)
  (with-temp-buffer
    (insert string)
    (delay-mode-hooks
      (c-mode)
      (run-hooks 'font-lock-mode-hook)
      (font-lock-fontify-buffer)
      (buffer-string))))

(defun robe-toggle-source (button)
  (let* ((end (button-end button))
         (value (get-text-property end 'invisible))
         (inhibit-read-only t))
    (put-text-property end (point-max) 'invisible (not value))))

(defun robe-signature (spec &optional arg-num)
  (concat
   (mapconcat (lambda (s) (propertize s 'face font-lock-type-face))
              (split-string (or (robe-spec-module spec) "?") "::" t) "::")
   (if (robe-spec-inst-p spec) "#" ".")
   (propertize (robe-spec-method spec) 'face font-lock-function-name-face)
   (robe-signature-params (robe-spec-params spec) arg-num)))

(defun robe-signature-params (params &optional arg-num)
  (when params
    (let ((cnt 0) args)
      (dolist (pair params)
        (let ((kind (intern (car pair)))
              (name (nth 1 pair)))
          (cl-incf cnt)
          (unless name
            (setq name
                  (cl-case kind
                    (rest "args")
                    (block "block")
                    (t (format "arg%s" cnt)))))
          (push (propertize (format (cl-case kind
                                      (rest "%s...")
                                      (block "&%s")
                                      (opt "[%s]")
                                      (keyreq "%s:")
                                      (key "[%s:]")
                                      (t "%s")) name)
                            'face (if (and arg-num
                                           (not (memq kind '(keyreq key)))
                                           (or (= arg-num cnt)
                                               (and (eq kind 'rest)
                                                    (> arg-num cnt))))
                                      (list robe-em-face 'bold)
                                    robe-em-face))
                args)))
      (concat "(" (mapconcat #'identity (nreverse args) ", ") ")"))))

(defun robe-doc-for (spec)
  (apply #'robe-request "doc_for" (cl-subseq spec 0 3)))

(defun robe-call-at-point ()
  (let ((state (syntax-ppss)) (start (point))
        in-arglist)
    (unless (nth 4 state)
      (when (nth 3 state) (goto-char (nth 8 state)))
      (unless (ignore-errors (save-excursion
                               (eq ?. (char-before
                                       (beginning-of-thing 'symbol)))))
        (when (or (robe-call-goto-paren state)
                  (robe-call-goto-parenless))
          (setq in-arglist t)))
      (let ((thing (thing-at-point 'symbol)))
        (when (and thing
                   (or (string= thing "super")
                       (not (memq (get-text-property 0 'face thing)
                                  (list font-lock-function-name-face
                                        font-lock-keyword-face)))))
          (cons thing (when in-arglist
                        (robe-call-arg-num (point) start))))))))

(defun robe-call-goto-paren (state)
  (when (and (cl-plusp (nth 0 state))
             (eq (char-after (nth 1 state)) ?\())
    (goto-char (nth 1 state))
    (skip-chars-backward " ")))

(defun robe-call-goto-parenless ()
  (let ((table (copy-syntax-table (syntax-table)))
        (punct (string-to-syntax "."))
        (start (point))
        point)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?@ "_" table)
    (with-syntax-table table
      (save-excursion
        (catch 'stop
          (unless (eobp) (forward-char 1))
          (while (re-search-backward "\\S-\\([ ]+\\)\\S-" nil t)
            (let ((state (parse-partial-sexp start (match-beginning 1))))
              (goto-char (match-beginning 1))
              (when (and (zerop (nth 0 state)) (not (nth 8 state)))
                (cond
                 ((save-match-data (string-match-p ";\\|=[^>]" (match-string 0)))
                  (throw 'stop t))
                 ((eq (char-after (match-beginning 0)) ?\n)
                  (unless (eq (char-before (match-beginning 0)) ?,)
                    (throw 'stop t)))
                 ((eq (char-after (match-beginning 0)) ?:) nil)
                 ((not (or (eq (syntax-after (match-beginning 0)) punct)
                           (eq (syntax-after (match-end 1)) punct)))
                  (setq point (match-beginning 1))
                  (throw 'stop t))))))))
      (when point (goto-char point)))))

(defun robe-call-arg-num (from point)
  (save-excursion
    (let ((depth (car (save-excursion (parse-partial-sexp from point))))
          (n 1))
      (while (re-search-forward "," point t)
        (let ((state (parse-partial-sexp from (point))))
          (when (and (= depth (car state)) (not (nth 8 state)))
            (cl-incf n))))
      n)))

(defun robe-eldoc ()
  (when (robe-running-p)
    (let* ((context nil)
           (call (save-excursion
                   (prog1
                       (robe-call-at-point)
                     (setq context (robe-call-context)))))
           (thing (car call))
           (arg-num (cdr call))
           (url-show-status nil))
      (when (and thing (not (robe-const-p thing)))
        (let* ((robe-jump-conservative t)
               (list (cl-loop for spec in (robe-jump-modules thing context)
                              when (robe-spec-module spec) collect spec)))
          (when (consp list)
            (let* ((spec (car list))
                   (doc (robe-doc-for spec))
                   (summary (with-temp-buffer
                              (insert (cdr (assoc 'docstring doc)))
                              (unless (= (point) (point-min))
                                (goto-char (point-min))
                                (save-excursion
                                  (forward-sentence)
                                  (delete-region (point) (point-max)))
                                (robe-doc-apply-rules (point) (point-max))
                                (while (search-forward "\n" nil t)
                                  (replace-match " ")))
                              (buffer-string)))
                   (sig (robe-signature spec arg-num))
                   (msg (format "%s %s" sig summary)))
              (substring msg 0 (min (frame-width) (length msg))))))))))

(defun robe-complete-symbol-p (beginning)
  (not (or (and
            ;; Implement symbol completion using Symbol.all_symbols.
            (eq (char-after beginning) ?:)
            (not (eq (char-after (1+ beginning)) ?:)))
           (memq (get-text-property beginning 'face)
                 (list font-lock-keyword-face
                       font-lock-function-name-face
                       font-lock-comment-face
                       font-lock-string-face)))))

(defun robe-complete-bounds ()
  (cons
   (save-excursion
     (while (or (not (zerop (skip-syntax-backward "w_")))
                (not (zerop (skip-chars-backward ":")))))
     (skip-chars-backward "@")
     (point))
   (save-excursion
     (while (or (not (zerop (skip-syntax-forward "w_")))
                (not (zerop (skip-chars-forward ":")))))
     (point))))

(defun robe-complete-at-point ()
  (when (robe-running-p)
    (let ((bounds (robe-complete-bounds))
          (fn (completion-table-with-cache #'robe-complete-thing)))
      (when (robe-complete-symbol-p (car bounds))
        (list (car bounds) (cdr bounds) fn
              :annotation-function #'robe-complete-annotation
              :exit-function #'robe-complete-exit)))))

(defvar robe-specs-cache nil)

(defun robe-cached-specs (method)
  (when robe-specs-cache
    (gethash method robe-specs-cache)))

(defun robe-complete-annotation (thing)
  (unless (get-text-property 0 'robe-type thing)
    (let ((params (robe-signature-params (robe-spec-params
                                          (car (robe-cached-specs thing))))))
      (if robe-highlight-capf-candidates
          params
        (and params
             (substring-no-properties params))))))

(defun robe-complete-exit (&rest _)
  (setq robe-specs-cache nil))

(defun robe-complete-thing (thing)
  (robe-start)
  (if (robe-const-p thing)
      (progn
        (robe-complete-exit)
        (robe-request "complete_const" thing (car (robe-context))))
    (cl-destructuring-bind
        (target module instance (_ instance-method? name)) (robe-call-context)
      (append
       (unless target
         ;; For company-robe mostly.  capf will call all-completions anyway.
         (all-completions
          thing
          (robe-complete--variables instance-method? name)))
       (robe-complete--methods thing target module instance)))))

(defun robe-complete--methods (thing target module instance)
  (setq robe-specs-cache (make-hash-table :test 'equal))
  (mapcar (lambda (spec)
            (let* ((method (robe-spec-method spec))
                   (value (gethash method robe-specs-cache))
                   case-fold-search)
              (puthash method (cons spec value) robe-specs-cache)
              (if robe-highlight-capf-candidates
                  (propertize method 'face
                              (if (string-match "\\`[A-Z]" method)
                                  'font-lock-type-face
                                'font-lock-function-name-face))
                method)))
          (reverse
           (robe-request "complete_method" thing target module instance))))

(defun robe-complete--variables (instance-method? method-name)
  (let ((instance-vars (and instance-method?
                            (robe-complete--instance-variables)))
        (local-vars (robe-complete--local-variables method-name)))
    (mapcar
     (lambda (str)
       (put-text-property 0 1 'robe-type 'variable str)
       str)
     (cl-delete-duplicates
      (nconc instance-vars local-vars)))))

(defun robe-complete--instance-variables ()
  (let ((bol (line-beginning-position))
        (eol (line-end-position))
        (var-regexp (rx
                     (or line-start (in ", \t"))
                     (group
                      (repeat 1 2 "@")
                      (+ (or (syntax ?w) (syntax ?_))))
                     (* ?\s)
                     ?=
                     (not (in "=>"))))
        vars)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward var-regexp bol t)
        (when (robe--not-in-string-or-comment)
          (push (match-string-no-properties 1) vars)))
      (goto-char eol)
      (while (re-search-forward var-regexp nil t)
        (when (robe--not-in-string-or-comment)
          (push (match-string-no-properties 1) vars))))
    vars))

(defun robe--not-in-string-or-comment ()
  (not (save-match-data (nth 8 (syntax-ppss)))))

(defun robe-complete--local-variables (method-name)
  (let ((method-regexp (concat
                        (rx
                         line-start
                         (* (in " \t"))
                         "def"
                         (* (in " \t"))
                         (optional (+ (in "a-z" "A-Z" ?:)) ?.))
                        method-name
                        (rx (* (in " \t"))
                            (or eol ?\( (syntax ?w)))))
        (block-regexp (rx
                       (or
                        (syntax ?w) (syntax ?_) ?\))
                       (+ (in " \t"))
                       (or
                        (sequence "do" symbol-end)
                        ?\{)
                       (* (in " \t"))
                       ?|
                       (group
                        (+ (not (in "|"))))
                       ?|))
        (arg-regexp (rx
                     (or point ?\( ?,)
                     (* (in " \t\n"))
                     (* ?*)
                     (group
                      (+ (or (syntax ?w) (syntax ?_))))))
        (var-regexp (rx
                     (or line-start (in ", \t"))
                     (group
                      (+ (or (syntax ?w) (syntax ?_))))
                     (* ?\s)
                     ?=
                     (not (in "=>"))))
        (bol (line-beginning-position))
        vars)
    ;; FIXME: Add support for block arguments.
    (save-excursion
      (when (and method-name
                 (re-search-backward method-regexp nil)
                 (eq ?\( (char-before (match-end 0))))
        (goto-char (1- (match-end 0)))
        (let* ((beg (1+ (point)))
               (forward-sexp-function nil)
               (end (progn
                      (forward-sexp 1)
                      (point))))
          (goto-char beg)
          (while (re-search-forward arg-regexp end t)
            (push (match-string-no-properties 1) vars))))
      (unless method-name
        (goto-char (point-min)))
      (save-excursion
        (while (re-search-forward block-regexp bol t)
          (when (robe--not-in-string-or-comment)
            (goto-char (match-beginning 1))
            (let ((end (match-end 1)))
              (while (re-search-forward arg-regexp end t)
                (push (match-string-no-properties 1) vars))))))
      ;; Now either after arglist or at bob.
      ;; FIXME: Also skip over blocks that do not contain
      ;; the original position.
      ;; `backward-up-list' can be slow-ish in large files,
      ;; but we could add a cache akin to syntax-ppss.
      (while (re-search-forward var-regexp bol t)
        (when (robe--not-in-string-or-comment)
          (push (match-string-no-properties 1) vars))))
    vars))

(defvar robe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'robe-jump)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    (define-key map (kbd "C-c C-d") 'robe-doc)
    (define-key map (kbd "C-c C-k") 'robe-rails-refresh)
    map))

;;;###autoload
(define-minor-mode robe-mode
  "Improved navigation for Ruby.

The following commands are available:

\\{robe-mode-map}"
  nil " robe" robe-mode-map
  (if robe-mode
      (progn
        (add-hook 'completion-at-point-functions 'robe-complete-at-point nil t)
        ;; Compose to work together with `yard-eldoc-message'
        ;; (though `yard-mode' has to be enabled first).
        ;; TODO: Adapt the code to use :before-until, for reliability.
        ;; TODO: Simplify when Emacs 25.1+ is required.
        (if eldoc-documentation-function
            (add-function :after-until (local 'eldoc-documentation-function)
                          #'robe-eldoc)
          (setq-local eldoc-documentation-function #'robe-eldoc)))
    (remove-hook 'completion-at-point-functions 'robe-complete-at-point t)
    (if (eq eldoc-documentation-function #'robe-eldoc)
        (kill-local-variable 'eldoc-documentation-function)
      (remove-function (local 'eldoc-documentation-function) #'robe-eldoc))))

(provide 'robe)
;;; robe.el ends here
