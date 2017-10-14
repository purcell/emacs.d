;;; counsel.el --- Various completion functions using Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Package-Version: 20171010.105
;; Version: 0.9.1
;; Package-Requires: ((emacs "24.3") (swiper "0.9.0"))
;; Keywords: completion, matching

;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Just call one of the interactive functions in this file to complete
;; the corresponding thing using `ivy'.
;;
;; Currently available:
;; - Symbol completion for Elisp, Common Lisp, Python and Clojure.
;; - Describe fuctions for Elisp: function, variable, library, command,
;;   bindings, theme.
;; - Navigation functions: imenu, ace-line, semantic, outline
;; - Git utilities: git-files, git-grep, git-log, git-stash.
;; - Grep utitilies: grep, ag, pt, recoll.
;; - System utilities: process list, rhythmbox, linux-app.
;; - Many more.

;;; Code:

(require 'swiper)
(require 'etags)
(require 'esh-util)

;;* Utility
(defun counsel-more-chars (n)
  "Return two fake candidates prompting for at least N input."
  (list ""
        (format "%d chars more" (- n (length ivy-text)))))

(defun counsel-unquote-regex-parens (str)
  "Unquote regex parenthesis in STR."
  (let ((start 0)
        ms)
    (while (setq start (string-match "\\\\)\\|\\\\(\\|[()]" str start))
      (setq ms (match-string-no-properties 0 str))
      (cond ((equal ms "\\(")
             (setq str (replace-match "(" nil t str))
             (setq start (+ start 1)))
            ((equal ms "\\)")
             (setq str (replace-match ")" nil t str))
             (setq start (+ start 1)))
            ((equal ms "(")
             (setq str (replace-match "\\(" nil t str))
             (setq start (+ start 2)))
            ((equal ms ")")
             (setq str (replace-match "\\)" nil t str))
             (setq start (+ start 2)))
            (t
             (error "Unexpected"))))
    str))

(defun counsel-directory-parent (dir)
  "Return the directory parent of directory DIR."
  (concat (file-name-nondirectory
           (directory-file-name dir)) "/"))

(defun counsel-string-compose (prefix str)
  "Make PREFIX the display prefix of STR through text properties."
  (let ((str (copy-sequence str)))
    (put-text-property
     0 1 'display
     (concat prefix (substring str 0 1))
     str)
    str))

(defun counsel-require-program (program)
  "Check system for PROGRAM, printing error if unfound."
  (when (and (stringp program)
             (not (string= program ""))
             (not (executable-find program)))
    (user-error "Required program \"%s\" not found in your path" program)))

;;* Async Utility
(defvar counsel--async-time nil
  "Store the time when a new process was started.
Or the time of the last minibuffer update.")

(defvar counsel--async-start nil
  "Store the time when a new process was started.
Or the time of the last minibuffer update.")

(defvar counsel--async-duration nil
  "Store the time a process takes to gather all its candidates.
The time is measured in seconds.")

(defvar counsel--async-exit-code-plist nil
  "Associates exit codes with reasons.")

(defun counsel-set-async-exit-code (cmd number str)
  "For CMD, associate NUMBER exit code with STR."
  (let ((plist (plist-get counsel--async-exit-code-plist cmd)))
    (setq counsel--async-exit-code-plist
          (plist-put
           counsel--async-exit-code-plist
           cmd
           (plist-put plist number str)))))

(defvar counsel-async-split-string-re "\n"
  "Store the regexp for splitting shell command output.")

(defvar counsel-async-ignore-re nil
  "Candidates matched the regexp will be ignored by `counsel--async-command'.")

(defun counsel--async-command (cmd &optional process-sentinel process-filter)
  "Start new counsel process by calling CMD.
If a counsel process is already running, kill it and its associated buffer
before starting a new one.  If non-nil, use PROCESS-SENTINEL as the sentinel
function instead of `counsel--async-sentinel'.  If non-nil, use PROCESS-FILTER
for handling the output of the process instead of `counsel--async-filter'."
  (let* ((counsel--process " *counsel*")
         (proc (get-process counsel--process))
         (buff (get-buffer counsel--process)))
    (when proc
      (delete-process proc))
    (when buff
      (kill-buffer buff))
    (setq buff (generate-new-buffer counsel--process))
    (setq proc (start-file-process-shell-command
                counsel--process
                counsel--process
                cmd))
    (setq counsel--async-start
          (setq counsel--async-time (current-time)))
    (set-process-sentinel proc (or process-sentinel #'counsel--async-sentinel))
    (set-process-filter proc (or process-filter #'counsel--async-filter))))

(defvar counsel-grep-last-line nil)

(defun counsel--async-sentinel (process event)
  "Sentinel function for an asynchronous counsel PROCESS.
EVENT is a string describing the change."
  (let ((cands
         (cond ((string= event "finished\n")
                (with-current-buffer (process-buffer process)
                  (split-string
                   (buffer-string)
                   counsel-async-split-string-re
                   t)))
               ((string-match "exited abnormally with code \\([0-9]+\\)\n" event)
                (let* ((exit-code-plist (plist-get counsel--async-exit-code-plist
                                                   (ivy-state-caller ivy-last)))
                       (exit-num (read (match-string 1 event)))
                       (exit-code (plist-get exit-code-plist exit-num)))
                  (list
                   (or exit-code
                       (format "error code %d" exit-num))))))))
    (cond ((string= event "finished\n")
           (ivy--set-candidates
            (ivy--sort-maybe
             cands))
           (setq counsel-grep-last-line nil)
           (when counsel--async-start
             (setq counsel--async-duration
                   (time-to-seconds (time-since counsel--async-start))))
           (let ((re (funcall ivy--regex-function ivy-text)))
             (unless (stringp re)
               (setq re (caar re)))
             (if (null ivy--old-cands)
                 (unless (ivy-set-index
                          (ivy--preselect-index
                           (ivy-state-preselect ivy-last)
                           ivy--all-candidates))
                   (ivy--recompute-index
                    ivy-text re ivy--all-candidates))
               (ivy--recompute-index
                ivy-text re ivy--all-candidates)))
           (setq ivy--old-cands ivy--all-candidates)
           (if (null ivy--all-candidates)
               (ivy--insert-minibuffer "")
             (ivy--exhibit)))
          ((string-match "exited abnormally with code \\([0-9]+\\)\n" event)
           (setq ivy--all-candidates cands)
           (setq ivy--old-cands ivy--all-candidates)
           (ivy--exhibit)))))

(defcustom counsel-async-filter-update-time 500000
  "The amount of time in microseconds to wait until updating
`counsel--async-filter'."
  :type 'integer
  :group 'ivy)

(defun counsel--async-filter (process str)
  "Receive from PROCESS the output STR.
Update the minibuffer with the amount of lines collected every
`counsel-async-filter-update-time' microseconds since the last update."
  (with-current-buffer (process-buffer process)
    (insert str))
  (let (size)
    (when (time-less-p
           `(0 0 ,counsel-async-filter-update-time 0)
           (time-since counsel--async-time))
      (with-current-buffer (process-buffer process)
        (goto-char (point-min))
        (setq size (- (buffer-size) (forward-line (buffer-size))))
        (ivy--set-candidates
         (let ((strings (split-string (buffer-string)
                                      counsel-async-split-string-re
                                      t)))
           (if (and counsel-async-ignore-re
                    (stringp counsel-async-ignore-re))
               (cl-remove-if
                (lambda (str)
                  (string-match-p counsel-async-ignore-re str))
                strings)
             strings))))
      (let ((ivy--prompt (format
                          (concat "%d++ " (ivy-state-prompt ivy-last))
                          size)))
        (ivy--insert-minibuffer
         (ivy--format ivy--all-candidates)))
      (setq counsel--async-time (current-time)))))

(defcustom counsel-prompt-function 'counsel-prompt-function-default
  "A function to return a full prompt string from a basic prompt string."
  :type
  '(choice
    (const :tag "Plain" counsel-prompt-function-default)
    (const :tag "Directory" counsel-prompt-function-dir)
    (function :tag "Custom"))
  :group 'ivy)

(make-obsolete-variable
 'counsel-prompt-function
 "Use `ivy-set-prompt' instead"
 "0.8.0 <2016-06-20 Mon>")

(defun counsel-prompt-function-default ()
  "Return prompt appended with a semicolon."
  (ivy-add-prompt-count
   (format "%s: " (ivy-state-prompt ivy-last))))

(defun counsel-delete-process ()
  "Delete current counsel process."
  (let ((process (get-process " *counsel*")))
    (when process
      (delete-process process))))

;;* Completion at point
;;** `counsel-el'
;;;###autoload
(defun counsel-el ()
  "Elisp completion at point."
  (interactive)
  (let* ((bnd (unless (and (looking-at ")")
                           (eq (char-before) ?\())
                (bounds-of-thing-at-point
                 'symbol)))
         (str (if bnd
                  (buffer-substring-no-properties
                   (car bnd)
                   (cdr bnd))
                ""))
         (ivy-height 7)
         (funp (eq (char-before (car bnd)) ?\())
         symbol-names)
    (if bnd
        (progn
          (setq ivy-completion-beg
                (move-marker (make-marker) (car bnd)))
          (setq ivy-completion-end
                (move-marker (make-marker) (cdr bnd))))
      (setq ivy-completion-beg nil)
      (setq ivy-completion-end nil))
    (if (string= str "")
        (mapatoms
         (lambda (x)
           (when (symbolp x)
             (push (symbol-name x) symbol-names))))
      (setq symbol-names
            (all-completions str obarray
                             (and funp
                                  (lambda (x)
                                    (or (functionp x)
                                        (macrop x)
                                        (special-form-p x)))))))
    (ivy-read "Symbol name: " symbol-names
              :predicate (and funp #'functionp)
              :initial-input str
              :action #'ivy-completion-in-region-action)))

;;** `counsel-cl'
(declare-function slime-symbol-start-pos "ext:slime")
(declare-function slime-symbol-end-pos "ext:slime")
(declare-function slime-contextual-completions "ext:slime-c-p-c")

;;;###autoload
(defun counsel-cl ()
  "Common Lisp completion at point."
  (interactive)
  (setq ivy-completion-beg (slime-symbol-start-pos))
  (setq ivy-completion-end (slime-symbol-end-pos))
  (ivy-read "Symbol name: "
            (car (slime-contextual-completions
                  ivy-completion-beg
                  ivy-completion-end))
            :action #'ivy-completion-in-region-action))

;;** `counsel-jedi'
(declare-function deferred:sync! "ext:deferred")
(declare-function jedi:complete-request "ext:jedi-core")
(declare-function jedi:ac-direct-matches "ext:jedi")

(defun counsel-jedi ()
  "Python completion at point."
  (interactive)
  (let ((bnd (bounds-of-thing-at-point 'symbol)))
    (if bnd
        (progn
          (setq ivy-completion-beg (car bnd))
          (setq ivy-completion-end (cdr bnd)))
      (setq ivy-completion-beg nil)
      (setq ivy-completion-end nil)))
  (deferred:sync!
      (jedi:complete-request))
  (ivy-read "Symbol name: " (jedi:ac-direct-matches)
            :action #'counsel--py-action))

(defun counsel--py-action (symbol)
  "Insert SYMBOL, erasing the previous one."
  (when (stringp symbol)
    (with-ivy-window
      (when ivy-completion-beg
        (delete-region
         ivy-completion-beg
         ivy-completion-end))
      (setq ivy-completion-beg
            (move-marker (make-marker) (point)))
      (insert symbol)
      (setq ivy-completion-end
            (move-marker (make-marker) (point)))
      (when (equal (get-text-property 0 'symbol symbol) "f")
        (insert "()")
        (setq ivy-completion-end
              (move-marker (make-marker) (point)))
        (backward-char 1)))))

;;** `counsel-clj'
(declare-function cider-sync-request:complete "ext:cider-client")
(defun counsel--generic (completion-fn)
  "Complete thing at point with COMPLETION-FN."
  (let* ((bnd (or (bounds-of-thing-at-point 'symbol)
                  (cons (point) (point))))
         (str (buffer-substring-no-properties
               (car bnd) (cdr bnd)))
         (candidates (funcall completion-fn str))
         (ivy-height 7)
         (res (ivy-read (format "pattern (%s): " str)
                        candidates)))
    (when (stringp res)
      (when bnd
        (delete-region (car bnd) (cdr bnd)))
      (insert res))))

;;;###autoload
(defun counsel-clj ()
  "Clojure completion at point."
  (interactive)
  (counsel--generic
   (lambda (str)
     (mapcar
      #'cl-caddr
      (cider-sync-request:complete str ":same")))))

;;** `counsel-unicode-char'
(defvar counsel-unicode-char-history nil
  "History for `counsel-unicode-char'.")

(defun counsel--unicode-names ()
  "Return formatted and sorted list of `ucs-names'.
The result of `ucs-names' is mostly, but not completely, sorted,
so this function ensures lexicographic order."
  (let* (cands
         (table (ucs-names))            ; Either hash map or alist
         (fmt   (lambda (name code)     ; Common format function
                  (push (propertize (format "%06X %-58s %c" code name code)
                                    'code code)
                        cands))))
    (if (not (hash-table-p table))
        ;; Support `ucs-names' returning an alist in Emacs < 26. The result of
        ;; `ucs-names' comes pre-reversed so no need to repeat.
        (dolist (entry table)
          (funcall fmt (car entry) (cdr entry)))
      (maphash fmt table)
      ;; Reverse to speed up sorting
      (setq cands (nreverse cands)))
    (sort cands #'string-lessp)))

(defvar counsel--unicode-table
  (lazy-completion-table counsel--unicode-table counsel--unicode-names)
  "Lazy completion table for `counsel-unicode-char'.
Candidates comprise `counsel--unicode-names', which see.")

;;;###autoload
(defun counsel-unicode-char (&optional count)
  "Insert COUNT copies of a Unicode character at point.
COUNT defaults to 1."
  (interactive "p")
  (let ((ivy-sort-max-size (expt 256 6)))
    (setq ivy-completion-beg (point))
    (setq ivy-completion-end (point))
    (ivy-read "Unicode name: " counsel--unicode-table
              :action (lambda (name)
                        (with-ivy-window
                          (delete-region ivy-completion-beg ivy-completion-end)
                          (setq ivy-completion-beg (point))
                          (insert-char (get-text-property 0 'code name) count)
                          (setq ivy-completion-end (point))))
              :history 'counsel-unicode-char-history
              :caller 'counsel-unicode-char
              :sort t)))

;;* Elisp symbols
;;** `counsel-describe-variable'
(defvar counsel-describe-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-.") #'counsel-find-symbol)
    (define-key map (kbd "C-,") #'counsel--info-lookup-symbol)
    map))

(ivy-set-actions
 'counsel-describe-variable
 '(("I" counsel-info-lookup-symbol "info")
   ("d" counsel--find-symbol "definition")))

(defvar counsel-describe-symbol-history nil
  "History for `counsel-describe-variable' and `counsel-describe-function'.")

(defun counsel-find-symbol ()
  "Jump to the definition of the current symbol."
  (interactive)
  (ivy-exit-with-action #'counsel--find-symbol))

(defun counsel--info-lookup-symbol ()
  "Lookup the current symbol in the info docs."
  (interactive)
  (ivy-exit-with-action #'counsel-info-lookup-symbol))

(defun counsel--find-symbol (x)
  "Find symbol definition that corresponds to string X."
  (with-ivy-window
    (with-no-warnings
      (ring-insert find-tag-marker-ring (point-marker)))
    (let ((full-name (get-text-property 0 'full-name x)))
      (if full-name
          (find-library full-name)
        (let ((sym (read x)))
          (cond ((and (eq (ivy-state-caller ivy-last)
                          'counsel-describe-variable)
                      (boundp sym))
                 (find-variable sym))
                ((fboundp sym)
                 (find-function sym))
                ((boundp sym)
                 (find-variable sym))
                ((or (featurep sym)
                     (locate-library
                      (prin1-to-string sym)))
                 (find-library
                  (prin1-to-string sym)))
                (t
                 (error "Couldn't find definition of %s"
                        sym))))))))

(define-obsolete-function-alias 'counsel-symbol-at-point
    'ivy-thing-at-point "0.7.0")

(defun counsel-variable-list ()
  "Return the list of all currently bound variables."
  (let (cands)
    (mapatoms
     (lambda (vv)
       (when (or (get vv 'variable-documentation)
                 (and (boundp vv) (not (keywordp vv))))
         (push (symbol-name vv) cands))))
    (delete "" cands)))

(defun counsel-describe-variable-transformer (var)
  "Propertize VAR if it's a custom variable."
  (if (custom-variable-p (intern var))
      (ivy-append-face var 'ivy-highlight-face)
    var))

(ivy-set-display-transformer
 'counsel-describe-variable 'counsel-describe-variable-transformer)

;;;###autoload
(defun counsel-describe-variable ()
  "Forward to `describe-variable'.

Variables declared using `defcustom' are highlighted according to
`ivy-highlight-face'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read
     "Describe variable: "
     (counsel-variable-list)
     :keymap counsel-describe-map
     :preselect (ivy-thing-at-point)
     :history 'counsel-describe-symbol-history
     :require-match t
     :sort t
     :action (lambda (x)
               (describe-variable
                (intern x)))
     :caller 'counsel-describe-variable)))

;;** `counsel-describe-function'
(ivy-set-actions
 'counsel-describe-function
 '(("I" counsel-info-lookup-symbol "info")
   ("d" counsel--find-symbol "definition")))

(defun counsel-describe-function-transformer (function-name)
  "Propertize FUNCTION-NAME if it's an interactive function."
  (if (commandp (intern function-name))
      (ivy-append-face function-name 'ivy-highlight-face)
    function-name))

(ivy-set-display-transformer
 'counsel-describe-function 'counsel-describe-function-transformer)

;;;###autoload
(defun counsel-describe-function ()
  "Forward to `describe-function'.

Interactive functions \(i.e., commands) are highlighted according
to `ivy-highlight-face'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Describe function: "
              (let (cands)
                (mapatoms
                 (lambda (x)
                   (when (fboundp x)
                     (push (symbol-name x) cands))))
                cands)
              :keymap counsel-describe-map
              :preselect (ivy-thing-at-point)
              :history 'counsel-describe-symbol-history
              :require-match t
              :sort t
              :action (lambda (x)
                        (describe-function
                         (intern x)))
              :caller 'counsel-describe-function)))

;;** `counsel-set-variable'
(defvar counsel-set-variable-history nil
  "Store history for `counsel-set-variable'.")

(defun counsel-read-setq-expression (sym)
  "Read and eval a setq expression for SYM."
  (setq this-command 'eval-expression)
  (let* ((minibuffer-completing-symbol t)
         (sym-value (symbol-value sym))
         (expr (minibuffer-with-setup-hook
                   (lambda ()
                     (add-function :before-until (local 'eldoc-documentation-function)
                                   #'elisp-eldoc-documentation-function)
                     (add-hook 'completion-at-point-functions #'elisp-completion-at-point nil t)
                     (run-hooks 'eval-expression-minibuffer-setup-hook)
                     (goto-char (minibuffer-prompt-end))
                     (forward-char 6)
                     (insert (format "%S " sym)))
                 (read-from-minibuffer "Eval: "
                                       (format
                                        (if (and sym-value (consp sym-value))
                                            "(setq '%S)"
                                          "(setq %S)")
                                        sym-value)
                                       read-expression-map t
                                       'read-expression-history))))
    (eval-expression expr)))

(defun counsel--setq-doconst (x)
  "Return a cons of description and value for X.
X is an item of a radio- or choice-type defcustom."
  (let (y)
    (when (and (listp x)
               (consp (setq y (last x))))
      (unless (equal y '(function))
        (setq x (car y))
        (cons (prin1-to-string x)
              (if (symbolp x)
                  (list 'quote x)
                x))))))

;;;###autoload
(defun counsel-set-variable ()
  "Set a variable, with completion.

When the selected variable is a `defcustom' with the type boolean
or radio, offer completion of all possible values.

Otherwise, offer a variant of `eval-expression', with the initial
input corresponding to the chosen variable."
  (interactive)
  (let ((sym (intern
              (ivy-read "Variable: "
                        (counsel-variable-list)
                        :preselect (ivy-thing-at-point)
                        :history 'counsel-set-variable-history)))
        sym-type
        cands)
    (if (and (boundp sym)
             (setq sym-type (get sym 'custom-type))
             (cond
               ((and (consp sym-type)
                     (memq (car sym-type) '(choice radio)))
                (setq cands (delq nil (mapcar #'counsel--setq-doconst (cdr sym-type)))))
               ((eq sym-type 'boolean)
                (setq cands '(("nil" . nil) ("t" . t))))
               (t nil)))
        (let* ((sym-val (symbol-value sym))
               ;; Escape '%' chars if present
               (sym-val-str (replace-regexp-in-string "%" "%%" (format "%s" sym-val)))
               (res (ivy-read (format "Set (%S <%s>): " sym sym-val-str)
                              cands
                              :preselect (prin1-to-string sym-val))))
          (when res
            (setq res
                  (if (assoc res cands)
                      (cdr (assoc res cands))
                    (read res)))
            (set sym (if (and (listp res) (eq (car res) 'quote))
                         (cadr res)
                       res))))
      (unless (boundp sym)
        (set sym nil))
      (counsel-read-setq-expression sym))))

;;** `counsel-info-lookup-symbol'
(defvar info-lookup-mode)
(declare-function info-lookup->completions "info-look")
(declare-function info-lookup->mode-value "info-look")
(declare-function info-lookup-select-mode "info-look")
(declare-function info-lookup-change-mode "info-look")
(declare-function info-lookup "info-look")

;;;###autoload
(defun counsel-info-lookup-symbol (symbol &optional mode)
  "Forward to (`info-lookup-symbol' SYMBOL MODE) with ivy completion."
  (interactive
   (progn
     (require 'info-look)
     (let* ((topic 'symbol)
            (mode (cond (current-prefix-arg
                         (info-lookup-change-mode topic))
                        ((info-lookup->mode-value
                          topic (info-lookup-select-mode))
                         info-lookup-mode)
                        ((info-lookup-change-mode topic))))
            (completions (info-lookup->completions topic mode))
            (enable-recursive-minibuffers t)
            (value (ivy-read
                    "Describe symbol: "
                    (mapcar #'car completions)
                    :preselect (ivy-thing-at-point)
                    :sort t)))
       (list value info-lookup-mode))))
  (require 'info-look)
  (info-lookup 'symbol symbol mode))

;;** `counsel-M-x'
(ivy-set-actions
 'counsel-M-x
 '(("d" counsel--find-symbol "definition")
   ("h" (lambda (x) (describe-function (intern x))) "help")))

(ivy-set-display-transformer
 'counsel-M-x
 'counsel-M-x-transformer)

;;;###autoload
(defun counsel-file-register ()
  "Search file in register.

You cannot use Emacs' normal register commands to create file
registers.  Instead you must use the `set-register' function like
so: `(set-register ?i \"/home/eric/.emacs.d/init.el\")'.  Now you
can use `C-x r j i' to open that file."
  (interactive)
  (ivy-read "File Register: "
            ;; Use the `register-alist' variable to filter out file
            ;; registers.  Each entry for a file registar will have the
            ;; following layout:
            ;;
            ;;     (NUMBER 'file . "string/path/to/file")
            ;;
            ;; So we go through each entry and see if the `cadr' is
            ;; `eq' to the symbol `file'.  If so then add the filename
            ;; (`cddr') which `ivy-read' will use for its choices.
            (mapcar (lambda (register-alist-entry)
                      (if (eq 'file (cadr register-alist-entry))
                          (cddr register-alist-entry)))
                      register-alist)
            :sort t
            :require-match t
            :history 'counsel-file-register
            :caller 'counsel-file-register
            :action (lambda (register-file)
                      (with-ivy-window (find-file register-file)))))

(ivy-set-actions
 'counsel-file-register
 '(("j" find-file-other-window "other window")))

(declare-function bookmark-all-names "bookmark")
(declare-function bookmark-location "bookmark")

(defcustom counsel-bookmark-avoid-dired nil
  "If non-nil, open directory bookmarks with `counsel-find-file'.
By default `counsel-bookmark' opens a dired buffer for directories."
  :type 'boolean
  :group 'ivy)

;;;###autoload
(defun counsel-bookmark ()
  "Forward to `bookmark-jump' or `bookmark-set' if bookmark doesn't exist."
  (interactive)
  (require 'bookmark)
  (ivy-read "Create or jump to bookmark: "
            (bookmark-all-names)
            :action (lambda (x)
                      (cond ((and counsel-bookmark-avoid-dired
                                  (member x (bookmark-all-names))
                                  (file-directory-p (bookmark-location x)))
                             (with-ivy-window
                               (let ((default-directory (bookmark-location x)))
                                 (counsel-find-file))))
                            ((member x (bookmark-all-names))
                             (with-ivy-window
                               (bookmark-jump x)))
                            (t
                             (bookmark-set x))))
            :caller 'counsel-bookmark))

(ivy-set-actions
 'counsel-bookmark
 '(("d" bookmark-delete "delete")
   ("e" bookmark-rename "edit")))

(defun counsel-M-x-transformer (cmd)
  "Return CMD appended with the corresponding binding in the current window."
  (let ((binding (substitute-command-keys (format "\\[%s]" cmd))))
    (setq binding (replace-regexp-in-string "C-x 6" "<f2>" binding))
    (if (string-match "^M-x" binding)
        cmd
      (format "%s (%s)"
              cmd (propertize binding 'face 'font-lock-keyword-face)))))

(defvar smex-initialized-p)
(defvar smex-ido-cache)
(declare-function smex-initialize "ext:smex")
(declare-function smex-detect-new-commands "ext:smex")
(declare-function smex-update "ext:smex")
(declare-function smex-rank "ext:smex")

(defun counsel--M-x-prompt ()
  "String for `M-x' plus the string representation of `current-prefix-arg'."
  (if (not current-prefix-arg)
      "M-x "
    (concat
     (if (eq current-prefix-arg '-)
         "- "
       (if (integerp current-prefix-arg)
           (format "%d " current-prefix-arg)
         (if (= (car current-prefix-arg) 4)
             "C-u "
           (format "%d " (car current-prefix-arg)))))
     "M-x ")))

(defvar counsel-M-x-history nil
  "History for `counsel-M-x'.")

;;;###autoload
(defun counsel-M-x (&optional initial-input)
  "Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer."
  (interactive)
  (let* ((cands obarray)
         (pred 'commandp)
         (sort t))
    (when (require 'smex nil 'noerror)
      (unless smex-initialized-p
        (smex-initialize))
      (when (smex-detect-new-commands)
        (smex-update))
      (setq cands smex-ido-cache)
      (setq pred nil)
      (setq sort nil))
    ;; When `counsel-M-x' returns, `last-command' would be set to
    ;; `counsel-M-x' because :action hasn't been invoked yet.
    ;; Instead, preserve the old value of `this-command'.
    (setq this-command last-command)
    (setq real-this-command real-last-command)
    (ivy-read (counsel--M-x-prompt) cands
              :predicate pred
              :require-match t
              :history 'counsel-M-x-history
              :action
              (lambda (cmd)
                (when (featurep 'smex)
                  (smex-rank (intern cmd)))
                (let ((prefix-arg current-prefix-arg))
                  (setq real-this-command
                        (setq this-command (intern cmd)))
                  (command-execute (intern cmd) 'record)))
              :sort sort
              :keymap counsel-describe-map
              :initial-input initial-input
              :caller 'counsel-M-x)))

;;** `counsel-load-library'
(defun counsel-library-candidates ()
  "Return a list of completion candidates for `counsel-load-library'."
  (interactive)
  (let ((dirs load-path)
        (suffix (concat (regexp-opt '(".el" ".el.gz") t) "\\'"))
        (cands (make-hash-table :test #'equal))
        short-name
        old-val
        dir-parent
        res)
    (dolist (dir dirs)
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions "" dir))
          (when (string-match suffix file)
            (unless (string-match "pkg.elc?$" file)
              (setq short-name (substring file 0 (match-beginning 0)))
              (if (setq old-val (gethash short-name cands))
                  (progn
                    ;; assume going up directory once will resolve name clash
                    (setq dir-parent (counsel-directory-parent (cdr old-val)))
                    (puthash short-name
                             (cons
                              (counsel-string-compose dir-parent (car old-val))
                              (cdr old-val))
                             cands)
                    (setq dir-parent (counsel-directory-parent dir))
                    (puthash (concat dir-parent short-name)
                             (cons
                              (propertize
                               (counsel-string-compose
                                dir-parent short-name)
                               'full-name (expand-file-name file dir))
                              dir)
                             cands))
                (puthash short-name
                         (cons (propertize
                                short-name
                                'full-name (expand-file-name file dir))
                               dir) cands)))))))
    (maphash (lambda (_k v) (push (car v) res)) cands)
    (nreverse res)))

;;;###autoload
(defun counsel-load-library ()
  "Load a selected the Emacs Lisp library.
The libraries are offered from `load-path'."
  (interactive)
  (let ((cands (counsel-library-candidates)))
    (ivy-read "Load library: " cands
              :action (lambda (x)
                        (load-library
                         (get-text-property 0 'full-name x)))
              :keymap counsel-describe-map)))

(ivy-set-actions
 'counsel-load-library
 '(("d" counsel--find-symbol "definition")))

;;** `counsel-find-library'
;;;###autoload
(defun counsel-find-library ()
  "Visit a selected the Emacs Lisp library.
The libraries are offered from `load-path'."
  (interactive)
  (let ((cands (counsel-library-candidates)))
    (ivy-read "Find library: " cands
              :action #'counsel--find-symbol
              :keymap counsel-describe-map)))

;;** `counsel-load-theme'
(declare-function powerline-reset "ext:powerline")

(defun counsel-load-theme-action (x)
  "Disable current themes and load theme X."
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (load-theme (intern x) t)
        (when (fboundp 'powerline-reset)
          (powerline-reset)))
    (error "Problem loading theme %s" x)))

;;;###autoload
(defun counsel-load-theme ()
  "Forward to `load-theme'.
Usable with `ivy-resume', `ivy-next-line-and-call' and
`ivy-previous-line-and-call'."
  (interactive)
  (ivy-read "Load custom theme: "
            (mapcar 'symbol-name
                    (custom-available-themes))
            :action #'counsel-load-theme-action
            :caller 'counsel-load-theme))

;;** `counsel-descbinds'
(ivy-set-actions
 'counsel-descbinds
 '(("d" counsel-descbinds-action-find "definition")
   ("I" counsel-descbinds-action-info "info")))

(defvar counsel-descbinds-history nil
  "History for `counsel-descbinds'.")

(defun counsel--descbinds-cands (&optional prefix buffer)
  "Get key bindings starting with PREFIX in BUFFER.
See `describe-buffer-bindings' for further information."
  (let ((buffer (or buffer (current-buffer)))
        (re-exclude (regexp-opt
                     '("<vertical-line>" "<bottom-divider>" "<right-divider>"
                       "<mode-line>" "<C-down-mouse-2>" "<left-fringe>"
                       "<right-fringe>" "<header-line>"
                       "<vertical-scroll-bar>" "<horizontal-scroll-bar>")))
        res)
    (with-temp-buffer
      (let ((indent-tabs-mode t))
        (describe-buffer-bindings buffer prefix))
      (goto-char (point-min))
      ;; Skip the "Key translations" section
      (re-search-forward "")
      (forward-char 1)
      (while (not (eobp))
        (when (looking-at "^\\([^\t\n]+\\)[\t ]*\\(.*\\)$")
          (let ((key (match-string 1))
                (fun (match-string 2))
                cmd)
            (unless (or (member fun '("??" "self-insert-command"))
                        (string-match re-exclude key)
                        (not (or (commandp (setq cmd (intern-soft fun)))
                                 (member fun '("Prefix Command")))))
              (push
               (cons (format
                      "%-15s %s"
                      (propertize key 'face 'font-lock-builtin-face)
                      fun)
                     (cons key cmd))
               res))))
        (forward-line 1)))
    (nreverse res)))

(defun counsel-descbinds-action-describe (x)
  "Describe function of candidate X.
See `describe-function' for further information."
  (let ((cmd (cddr x)))
    (describe-function cmd)))

(defun counsel-descbinds-action-find (x)
  "Find symbol definition of candidate X.
See `counsel--find-symbol' for further information."
  (let ((cmd (cddr x)))
    (counsel--find-symbol (symbol-name cmd))))

(defun counsel-descbinds-action-info (x)
  "Display symbol definition of candidate X, as found in the relevant manual.
See `info-lookup-symbol' for further information."
  (let ((cmd (cddr x)))
    (counsel-info-lookup-symbol (symbol-name cmd))))

;;;###autoload
(defun counsel-descbinds (&optional prefix buffer)
  "Show a list of all defined keys and their definitions.
If non-nil, show only bindings that start with PREFIX.
BUFFER defaults to the current one."
  (interactive)
  (ivy-read "Bindings: " (counsel--descbinds-cands prefix buffer)
            :action #'counsel-descbinds-action-describe
            :history 'counsel-descbinds-history
            :caller 'counsel-descbinds))
;;** `counsel-describe-face'
(defun counsel-describe-face ()
  "Completion for `describe-face'."
  (interactive)
  (let (cands)
    (mapatoms
     (lambda (s)
       (if (facep s)
           (push (symbol-name s) cands))))
    (ivy-read "Face: " cands
              :preselect (symbol-name (face-at-point t))
              :action #'describe-face)))
;;* Git
;;** `counsel-git'
(defvar counsel-git-cmd "git ls-files --full-name --"
  "Command for `counsel-git'.")

(defvar counsel--git-dir nil
  "Store the base git directory.")

(ivy-set-actions
 'counsel-git
 '(("j" find-file-other-window "other window")
   ("x" counsel-find-file-extern "open externally")))

(defun counsel-locate-git-root ()
  "Locate the root of the git repository containing the current buffer."
  (or (locate-dominating-file default-directory ".git")
      (error "Not in a git repository")))

;;;###autoload
(defun counsel-git (&optional initial-input)
  "Find file in the current Git repository.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (counsel-require-program (car (split-string counsel-git-cmd)))
  (ivy-set-prompt 'counsel-git counsel-prompt-function)
  (setq counsel--git-dir (expand-file-name
                          (counsel-locate-git-root)))
  (let* ((default-directory counsel--git-dir)
         (cands (split-string
                 (shell-command-to-string counsel-git-cmd)
                 "\n"
                 t)))
    (ivy-read "Find file" cands
              :initial-input initial-input
              :action #'counsel-git-action
              :caller 'counsel-git)))

(defun counsel-git-action (x)
  "Find file X in current Git repository."
  (with-ivy-window
    (let ((default-directory counsel--git-dir))
      (find-file x))))

;;** `counsel-git-grep'
(defvar counsel-git-grep-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'ivy-call-and-recenter)
    (define-key map (kbd "M-q") 'counsel-git-grep-query-replace)
    (define-key map (kbd "C-c C-m") 'counsel-git-grep-switch-cmd)
    map))

(ivy-set-occur 'counsel-git-grep 'counsel-git-grep-occur)
(ivy-set-display-transformer 'counsel-git-grep 'counsel-git-grep-transformer)

(defvar counsel-git-grep-cmd-default "git --no-pager grep --full-name -n --no-color -i -e \"%s\""
  "Initial command for `counsel-git-grep'.")

(defvar counsel-git-grep-cmd nil
  "Store the command for `counsel-git-grep'.")

(defvar counsel--git-grep-count nil
  "Store the line count in current repository.")

(defvar counsel-git-grep-history nil
  "History for `counsel-git-grep'.")

(defvar counsel-git-grep-cmd-history
  (list counsel-git-grep-cmd-default)
  "History for `counsel-git-grep' shell commands.")

(defcustom counsel-grep-post-action-hook nil
  "Hook that runs after the point moves to the next candidate.
Typical value: '(recenter)."
  :type 'hook
  :group 'ivy)

(defun counsel-prompt-function-dir ()
  "Return prompt appended with the parent directory."
  (ivy-add-prompt-count
   (let ((directory counsel--git-dir))
     (format "%s [%s]: "
             (ivy-state-prompt ivy-last)
             (let ((dir-list (eshell-split-path directory)))
               (if (> (length dir-list) 3)
                   (apply #'concat
                          (append '("...")
                                  (cl-subseq dir-list (- (length dir-list) 3))))
                 directory))))))

(defcustom counsel-git-grep-skip-counting-lines nil
  "If non-nil, don't count lines before grepping ina git repository."
  :type 'boolean
  :group 'ivy)

(defun counsel-git-grep-function (string &optional _pred &rest _unused)
  "Grep in the current git repository for STRING."
  (if (and (or counsel-git-grep-skip-counting-lines (> counsel--git-grep-count 20000))
           (< (length string) 3))
      (counsel-more-chars 3)
    (let* ((default-directory counsel--git-dir)
           (cmd (format counsel-git-grep-cmd
                        (setq ivy--old-re (ivy--regex string t)))))
      (if (and (not counsel-git-grep-skip-counting-lines) (<= counsel--git-grep-count 20000))
          (split-string (shell-command-to-string cmd) "\n" t)
        (counsel--gg-candidates (ivy--regex string))
        nil))))

(defun counsel-git-grep-action (x)
  "Go to occurrence X in current Git repository."
  (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
    (with-ivy-window
      (let ((file-name (match-string-no-properties 1 x))
            (line-number (match-string-no-properties 2 x)))
        (find-file (expand-file-name file-name counsel--git-dir))
        (goto-char (point-min))
        (forward-line (1- (string-to-number line-number)))
        (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
        (swiper--ensure-visible)
        (run-hooks 'counsel-grep-post-action-hook)
        (unless (eq ivy-exit 'done)
          (swiper--cleanup)
          (swiper--add-overlays (ivy--regex ivy-text)))))))

(defun counsel-git-grep-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES for `counsel-git-grep'."
  (or (and (equal regexp ivy--old-re)
           ivy--old-cands)
      (prog1
          (setq ivy--old-cands
                (cl-remove-if-not
                 (lambda (x)
                   (ignore-errors
                     (when (string-match "^[^:]+:[^:]+:" x)
                       (setq x (substring x (match-end 0)))
                       (if (stringp regexp)
                           (string-match regexp x)
                         (let ((res t))
                           (dolist (re regexp)
                             (setq res
                                   (and res
                                        (ignore-errors
                                          (if (cdr re)
                                              (string-match (car re) x)
                                            (not (string-match (car re) x)))))))
                           res)))))
                 candidates))
        (setq ivy--old-re regexp))))

(defun counsel-git-grep-transformer (str)
  "Higlight file and line number in STR."
  (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):" str)
    (set-text-properties (match-beginning 1)
                         (match-end 1)
                         '(face compilation-info)
                         str)
    (set-text-properties (match-beginning 2)
                         (match-end 2)
                         '(face compilation-line-number)
                         str))
  str)

(defvar counsel-git-grep-projects-alist nil
  "An alist of project directory to \"git-grep\" command.
Allows to automatically use a custom \"git-grep\" command for all
files in a project.")

(defun counsel--git-grep-cmd-and-proj (cmd)
  (let ((dd (expand-file-name default-directory))
        proj)
    (cond
      ((stringp cmd))
      (cmd
       (if (setq proj
                 (cl-find-if
                  (lambda (x)
                    (string-match (car x) dd))
                  counsel-git-grep-projects-alist))
           (setq cmd (cdr proj))
         (setq cmd
               (ivy-read "cmd: " counsel-git-grep-cmd-history
                         :history 'counsel-git-grep-cmd-history
                         :re-builder #'ivy--regex))
         (setq counsel-git-grep-cmd-history
               (delete-dups counsel-git-grep-cmd-history))))
      (t
       (setq cmd counsel-git-grep-cmd-default)))
    (cons proj cmd)))

;;;###autoload
(defun counsel-git-grep (&optional cmd initial-input)
  "Grep for a string in the current git repository.
When CMD is a string, use it as a \"git grep\" command.
When CMD is non-nil, prompt for a specific \"git grep\" command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive "P")
  (ivy-set-prompt 'counsel-git-grep counsel-prompt-function)
  (let ((proj-and-cmd (counsel--git-grep-cmd-and-proj cmd))
        proj)
    (setq proj (car proj-and-cmd))
    (setq counsel-git-grep-cmd (cdr proj-and-cmd))
    (counsel-require-program (car (split-string counsel-git-grep-cmd)))
    (setq counsel--git-dir (if proj
                               (car proj)
                             (counsel-locate-git-root)))
    (unless (or proj counsel-git-grep-skip-counting-lines)
      (setq counsel--git-grep-count
            (if (eq system-type 'windows-nt)
                0
              (counsel--gg-count "" t))))
    (let ((collection-function
           (if proj
               #'counsel-git-grep-proj-function
             #'counsel-git-grep-function))
          (unwind-function
           (if proj
               (lambda ()
                 (counsel-delete-process)
                 (swiper--cleanup))
             (lambda ()
               (swiper--cleanup)))))
      (ivy-read "git grep" collection-function
                :initial-input initial-input
                :matcher #'counsel-git-grep-matcher
                :dynamic-collection (or proj counsel-git-grep-skip-counting-lines (> counsel--git-grep-count 20000))
                :keymap counsel-git-grep-map
                :action #'counsel-git-grep-action
                :unwind unwind-function
                :history 'counsel-git-grep-history
                :caller 'counsel-git-grep))))

(defun counsel-git-grep-proj-function (str)
  "Grep for STR in the current git repository."
  (if (< (length str) 3)
      (counsel-more-chars 3)
    (let ((regex (setq ivy--old-re
                       (ivy--regex str t))))
      (counsel--async-command (format counsel-git-grep-cmd regex))
      nil)))

(defun counsel-git-grep-switch-cmd ()
  "Set `counsel-git-grep-cmd' to a different value."
  (interactive)
  (setq counsel-git-grep-cmd
        (ivy-read "cmd: " counsel-git-grep-cmd-history
                  :history 'counsel-git-grep-cmd-history))
  (setq counsel-git-grep-cmd-history
        (delete-dups counsel-git-grep-cmd-history))
  (unless (ivy-state-dynamic-collection ivy-last)
    (setq ivy--all-candidates
          (all-completions "" 'counsel-git-grep-function))))

(defvar counsel-gg-state nil
  "The current state of candidates / count sync.")

(defun counsel--gg-candidates (regex)
  "Return git grep candidates for REGEX."
  (setq counsel-gg-state -2)
  (counsel--gg-count regex)
  (let* ((default-directory counsel--git-dir)
         (counsel-gg-process " *counsel-gg*")
         (proc (get-process counsel-gg-process))
         (buff (get-buffer counsel-gg-process)))
    (when proc
      (delete-process proc))
    (when buff
      (kill-buffer buff))
    (setq proc (start-process-shell-command
                counsel-gg-process
                counsel-gg-process
                (concat
                 (format counsel-git-grep-cmd regex)
                 " | head -n 200")))
    (set-process-sentinel
     proc
     #'counsel--gg-sentinel)))

(defun counsel--gg-sentinel (process event)
  "Sentinel function for a `counsel-git-grep' PROCESS.
EVENT is a string describing the change."
  (if (member event '("finished\n"
                      "exited abnormally with code 141\n"))
      (progn
        (with-current-buffer (process-buffer process)
          (setq ivy--all-candidates
                (or (split-string (buffer-string) "\n" t)
                    '("")))
          (setq ivy--old-cands ivy--all-candidates))
        (when (= 0 (cl-incf counsel-gg-state))
          (ivy--exhibit)))
    (if (string= event "exited abnormally with code 1\n")
        (progn
          (setq ivy--all-candidates '("Error"))
          (setq ivy--old-cands ivy--all-candidates)
          (ivy--exhibit)))))

(defun counsel--gg-count (regex &optional no-async)
  "Count the number of results matching REGEX in `counsel-git-grep'.
The command to count the matches is called asynchronously.
If NO-ASYNC is non-nil, do it synchronously instead."
  (let ((default-directory counsel--git-dir)
        (cmd
         (concat
          (format
           (replace-regexp-in-string
            "--full-name" "-c"
            counsel-git-grep-cmd)
           ;; "git grep -i -c '%s'"
           (replace-regexp-in-string
            "-" "\\\\-"
            (replace-regexp-in-string "'" "''" regex)))
          " | sed 's/.*:\\(.*\\)/\\1/g' | awk '{s+=$1} END {print s}'"))
        (counsel-ggc-process " *counsel-gg-count*"))
    (if no-async
        (string-to-number (shell-command-to-string cmd))
      (let ((proc (get-process counsel-ggc-process))
            (buff (get-buffer counsel-ggc-process)))
        (when proc
          (delete-process proc))
        (when buff
          (kill-buffer buff))
        (setq proc (start-process-shell-command
                    counsel-ggc-process
                    counsel-ggc-process
                    cmd))
        (set-process-sentinel
         proc
         #'(lambda (process event)
             (when (string= event "finished\n")
               (with-current-buffer (process-buffer process)
                 (setq ivy--full-length (string-to-number (buffer-string))))
               (when (= 0 (cl-incf counsel-gg-state))
                 (ivy--exhibit)))))))))

(defun counsel-git-grep-occur ()
  "Generate a custom occur buffer for `counsel-git-grep'.
When REVERT is non-nil, regenerate the current *ivy-occur* buffer."
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode)
    (setq default-directory counsel--git-dir))
  (setq ivy-text
        (and (string-match "\"\\(.*\\)\"" (buffer-name))
             (match-string 1 (buffer-name))))
  (let* ((regex (funcall ivy--regex-function ivy-text))
         (positive-pattern (replace-regexp-in-string
                            ;; git-grep can't handle .*?
                            "\\.\\*\\?" ".*"
                            (if (stringp regex) regex (caar regex))))
         (negative-patterns
          (if (stringp regex) ""
            (mapconcat (lambda (x)
                         (and (null (cdr x))
                              (format "| grep -v %s" (car x))))
                       regex
                       " ")))
         (cmd (concat (format counsel-git-grep-cmd positive-pattern) negative-patterns))
         cands)
    (setq cands (split-string
                 (shell-command-to-string cmd)
                 "\n"
                 t))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines
     (mapcar
      (lambda (cand) (concat "./" cand))
      cands))))

(defun counsel-git-grep-query-replace ()
  "Start `query-replace' with string to replace from last search string."
  (interactive)
  (if (null (window-minibuffer-p))
      (user-error
       "Should only be called in the minibuffer through `counsel-git-grep-map'")
    (let* ((enable-recursive-minibuffers t)
           (from (ivy--regex ivy-text))
           (to (query-replace-read-to from "Query replace" t)))
      (ivy-exit-with-action
       (lambda (_)
         (let (done-buffers)
           (dolist (cand ivy--old-cands)
             (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
               (with-ivy-window
                 (let ((file-name (match-string-no-properties 1 cand)))
                   (setq file-name (expand-file-name file-name counsel--git-dir))
                   (unless (member file-name done-buffers)
                     (push file-name done-buffers)
                     (find-file file-name)
                     (goto-char (point-min)))
                   (perform-replace from to t t nil)))))))))))

;;** `counsel-git-stash'
(defun counsel-git-stash-kill-action (x)
  "Add git stash command to kill ring.
The git command applies the stash entry where candidate X was found in."
  (when (string-match "\\([^:]+\\):" x)
    (kill-new (message (format "git stash apply %s" (match-string 1 x))))))

;;;###autoload
(defun counsel-git-stash ()
  "Search through all available git stashes."
  (interactive)
  (setq counsel--git-dir (counsel-locate-git-root))
  (let ((cands (split-string (shell-command-to-string
                              "IFS=$'\n'
for i in `git stash list --format=\"%gd\"`; do
    git stash show -p $i | grep -H --label=\"$i\" \"$1\"
done") "\n" t)))
    (ivy-read "git stash: " cands
              :action 'counsel-git-stash-kill-action
              :caller 'counsel-git-stash)))

;;** `counsel-git-log'
(defvar counsel-git-log-cmd "GIT_PAGER=cat git log --grep '%s'"
  "Command used for \"git log\".")

(defvar counsel-git-log-split-string-re "\ncommit "
  "The `split-string' separates when split output of `counsel-git-log-cmd'.")

(defun counsel-git-log-function (input)
  "Search for INPUT in git log."
  (if (< (length input) 3)
      (counsel-more-chars 3)
    ;; `counsel--yank-pop-format-function' uses this
    (setq ivy--old-re (funcall ivy--regex-function input))
    (counsel--async-command
     ;; "git log --grep" likes to have groups quoted e.g. \(foo\).
     ;; But it doesn't like the non-greedy ".*?".
     (format counsel-git-log-cmd
             (replace-regexp-in-string "\\.\\*\\?" ".*"
                                       (ivy-re-to-str ivy--old-re))))
    nil))

(defun counsel-git-log-action (x)
  "Add candidate X to kill ring."
  (message "%S" (kill-new x)))

(defcustom counsel-yank-pop-truncate-radius 2
  "When non-nil, truncate the display of long strings."
  :type 'integer
  :group 'ivy)

;;** `counsel-git-change-worktree'
(autoload 'string-trim-right "subr-x")
(defun counsel-git-change-worktree-action (git-root-dir tree)
  "Find the corresponding file in the worktree located at tree.
The current buffer is assumed to be in a subdirectory of GIT-ROOT-DIR.
TREE is the selected candidate."
  (let* ((new-root-dir (counsel-git-worktree-parse-root tree))
         (tree-filename (file-relative-name (buffer-file-name) git-root-dir))
         (file-name (expand-file-name tree-filename new-root-dir)))
    (find-file file-name)))

(defun counsel-git-worktree-list ()
  "List worktrees in the git repository containing the current buffer."
  (setq counsel--git-dir (counsel-locate-git-root))
  (let ((cmd-output (shell-command-to-string "git worktree list")))
    (delete "" (split-string (string-trim-right cmd-output) "\n"))))

(defun counsel-git-worktree-parse-root (tree)
  "Return worktree from candidate TREE."
  (substring tree 0 (string-match " " tree)))

(defun counsel-git-close-worktree-files-action (root-dir)
  "Close all buffers from the worktree located at ROOT-DIR."
  (setq root-dir (counsel-git-worktree-parse-root root-dir))
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (and buffer-file-name
           (string= "." (file-relative-name root-dir (counsel-locate-git-root)))
           (kill-buffer buf)))))

(ivy-set-actions
 'counsel-git-change-worktree
 '(("k" counsel-git-close-worktree-files-action "kill all")))

;;;###autoload
(defun counsel-git-change-worktree ()
  "Find the file corresponding to the current buffer on a different worktree."
  (interactive)
  (setq counsel--git-dir (counsel-locate-git-root))
  (ivy-read "Select worktree: "
            (or (cl-delete counsel--git-dir (counsel-git-worktree-list)
                           :key #'counsel-git-worktree-parse-root :test #'string=)
                (error "No other worktrees!"))
            :action (lambda (tree) (counsel-git-change-worktree-action counsel--git-dir tree))
            :require-match t
            :caller 'counsel-git-change-worktree))

;;** `counsel-git-checkout'
(defun counsel-git-checkout-action (branch)
  "Call the \"git checkout BRANCH\" command.

BRANCH is a string whose first word designates the command argument."
  (shell-command
   (format "git checkout %s" (substring branch 0 (string-match " " branch)))))

(defun counsel-git-branch-list ()
  "List branches in the git repository containing the current buffer.

Does not list the currently checked out one."
  (setq counsel--git-dir (counsel-locate-git-root))
  (let ((cmd-output (shell-command-to-string "git branch -vv --all")))
    (cl-mapcan
     (lambda (str)
       (when (string-prefix-p " " str)
         (list (substring str (string-match "[^[:blank:]]" str)))))
     (split-string (string-trim-right cmd-output) "\n"))))

;;;###autoload
(defun counsel-git-checkout ()
  "Call the \"git checkout\" command."
  (interactive)
  (ivy-read "Checkout branch: " (counsel-git-branch-list)
            :action #'counsel-git-checkout-action
            :caller 'counsel-git-checkout))

;;;###autoload
(defun counsel-git-log ()
  "Call the \"git log --grep\" shell command."
  (interactive)
  (let ((counsel-async-split-string-re counsel-git-log-split-string-re)
        (counsel-async-ignore-re "^[ \n]*$")
        (counsel-yank-pop-truncate-radius 5)
        (ivy-format-function #'counsel--yank-pop-format-function)
        (ivy-height 4))
    (ivy-read "Grep log: " #'counsel-git-log-function
              :dynamic-collection t
              :action #'counsel-git-log-action
              :unwind #'counsel-delete-process
              :caller 'counsel-git-log)))

;;* File
;;** `counsel-find-file'
(defvar counsel-find-file-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-DEL") 'counsel-up-directory)
    (define-key map (kbd "C-<backspace>") 'counsel-up-directory)
    map))

(when (executable-find "git")
  (add-to-list 'ivy-ffap-url-functions 'counsel-github-url-p)
  (add-to-list 'ivy-ffap-url-functions 'counsel-emacs-url-p))
(add-to-list 'ivy-ffap-url-functions 'counsel-url-expand)
(defun counsel-find-file-cd-bookmark-action (_)
  "Reset `counsel-find-file' from selected directory."
  (ivy-read "cd: "
            (progn
              (ivy--virtual-buffers)
              (delete-dups
               (mapcar (lambda (x) (file-name-directory (cdr x)))
                       ivy--virtual-buffers)))
            :action (lambda (x)
                      (let ((default-directory (file-name-directory x)))
                        (counsel-find-file)))))

(defcustom counsel-root-command "sudo"
  "Command to gain root privileges."
  :type 'string
  :group 'ivy)

(defun counsel-find-file-as-root (x)
  "Find file X with root privileges."
  (counsel-require-program counsel-root-command)
  (let* ((host (file-remote-p x 'host))
         (file-name (format "/%s:%s:%s"
                            counsel-root-command
                            (or host "")
                            (expand-file-name
                             (if host
                                 (file-remote-p x 'localname)
                               x)))))
    ;; If the current buffer visits the same file we are about to open,
    ;; replace the current buffer with the new one.
    (if (eq (current-buffer) (get-file-buffer x))
        (find-alternate-file file-name)
      (find-file file-name))))

(ivy-set-actions
 'counsel-find-file
 '(("j" find-file-other-window "other window")
   ("b" counsel-find-file-cd-bookmark-action "cd bookmark")
   ("x" counsel-find-file-extern "open externally")
   ("r" counsel-find-file-as-root "open as root")))

(defcustom counsel-find-file-at-point nil
  "When non-nil, add file-at-point to the list of candidates."
  :type 'boolean
  :group 'ivy)

(defcustom counsel-find-file-ignore-regexp nil
  "A regexp of files to ignore while in `counsel-find-file'.
These files are un-ignored if `ivy-text' matches them.  The
common way to show all files is to start `ivy-text' with a dot.

Example value: \"\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)\".  This will hide
temporary and lock files.
\\<ivy-minibuffer-map>
Choosing the dotfiles option, \"\\`\\.\", might be convenient,
since you can still access the dotfiles if your input starts with
a dot. The generic way to toggle ignored files is \\[ivy-toggle-ignore],
but the leading dot is a lot faster."
  :group 'ivy
  :type `(choice
          (const :tag "None" nil)
          (const :tag "Dotfiles" "\\`\\.")
          (const :tag "Ignored Extensions"
                 ,(regexp-opt completion-ignored-extensions))
          (regexp :tag "Regex")))

(defun counsel--find-file-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES.
Skip some dotfiles unless `ivy-text' requires them."
  (let ((res (ivy--re-filter regexp candidates)))
    (if (or (null ivy-use-ignore)
            (null counsel-find-file-ignore-regexp)
            (string-match "\\`\\." ivy-text))
        res
      (or (cl-remove-if
           (lambda (x)
             (and
              (string-match counsel-find-file-ignore-regexp x)
              (not (member x ivy-extra-directories))))
           res)
          res))))

(declare-function ffap-guesser "ffap")

(defvar counsel-find-file-speedup-remote t
  "Speed up opening remote files by disabling `find-file-hook' for them.")

;;;###autoload
(defun counsel-find-file (&optional initial-input)
  "Forward to `find-file'.
When INITIAL-INPUT is non-nil, use it in the minibuffer during completion."
  (interactive)
  (ivy-read "Find file: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action
            (lambda (x)
              (with-ivy-window
                (if (and counsel-find-file-speedup-remote
                         (file-remote-p ivy--directory))
                    (let ((find-file-hook nil))
                      (find-file (expand-file-name x ivy--directory)))
                  (find-file (expand-file-name x ivy--directory)))))
            :preselect (when counsel-find-file-at-point
                         (require 'ffap)
                         (let ((f (ffap-guesser)))
                           (when f (expand-file-name f))))
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller 'counsel-find-file))

(defun counsel-up-directory ()
  "Go to the parent directory preselecting the current one."
  (interactive)
  (let ((dir-file-name
         (directory-file-name (expand-file-name ivy--directory))))
    (ivy--cd (file-name-directory dir-file-name))
    (setf (ivy-state-preselect ivy-last)
          (file-name-as-directory (file-name-nondirectory dir-file-name)))))

(defun counsel-at-git-issue-p ()
  "When point is at an issue in a Git-versioned file, return the issue string."
  (and (looking-at "#[0-9]+")
       (or
        (eq (vc-backend (buffer-file-name)) 'Git)
        (or
         (memq major-mode '(magit-commit-mode))
         (bound-and-true-p magit-commit-mode)))
       (match-string-no-properties 0)))

(defun counsel-github-url-p ()
  "Return a Github issue URL at point."
  (counsel-require-program "git")
  (let ((url (counsel-at-git-issue-p)))
    (when url
      (let ((origin (shell-command-to-string
                     "git remote get-url origin"))
            user repo)
        (cond ((string-match "\\`git@github.com:\\([^/]+\\)/\\(.*\\)\\.git$"
                             origin)
               (setq user (match-string 1 origin))
               (setq repo (match-string 2 origin)))
              ((string-match "\\`https://github.com/\\([^/]+\\)/\\(.*\\)$"
                             origin)
               (setq user (match-string 1 origin))
               (setq repo (match-string 2 origin))))
        (when user
          (setq url (format "https://github.com/%s/%s/issues/%s"
                            user repo (substring url 1))))))))

(defun counsel-emacs-url-p ()
  "Return a Debbugs issue URL at point."
  (counsel-require-program "git")
  (let ((url (counsel-at-git-issue-p)))
    (when url
      (let ((origin (shell-command-to-string
                     "git remote get-url origin")))
        (when (string-match "git.sv.gnu.org:/srv/git/emacs.git" origin)
          (format "http://debbugs.gnu.org/cgi/bugreport.cgi?bug=%s"
                  (substring url 1)))))))

(defvar counsel-url-expansions-alist nil
  "Map of regular expressions to expansions.

This variable should take the form of a list of (REGEXP . FORMAT)
pairs.

`counsel-url-expand' will expand the word at point according to
FORMAT for the first matching REGEXP.  FORMAT can be either a
string or a function.  If it is a string, it will be used as the
format string for the `format' function, with the word at point
as the next argument.  If it is a function, it will be called
with the word at point as the sole argument.

For example, a pair of the form:
  '(\"\\`BSERV-[[:digit:]]+\\'\" . \"https://jira.atlassian.com/browse/%s\")
will expand to URL `https://jira.atlassian.com/browse/BSERV-100'
when the word at point is BSERV-100.

If the format element is a function, more powerful
transformations are possible.  As an example,
  '(\"\\`issue\\([[:digit:]]+\\)\\'\" .
    (lambda (word)
      (concat \"http://debbugs.gnu.org/cgi/bugreport.cgi?bug=\"
              (match-string 1 word))))
trims the \"issue\" prefix from the word at point before creating the URL.")

(defun counsel-url-expand ()
  "Expand word at point using `counsel-url-expansions-alist'.
The first pair in the list whose regexp matches the word at point
will be expanded according to its format.  This function is
intended to be used in `ivy-ffap-url-functions' to browse the
result as a URL."
  (let ((word-at-point (current-word)))
    (cl-some
     (lambda (pair)
       (let ((regexp (car pair))
             (formatter (cdr pair)))
         (when (string-match regexp word-at-point)
           (if (functionp formatter)
               (funcall formatter word-at-point)
             (format formatter word-at-point)))))
     counsel-url-expansions-alist)))

;;** `counsel-recentf'
(defvar recentf-list)
(declare-function recentf-mode "recentf")

;;;###autoload
(defun counsel-recentf ()
  "Find a file on `recentf-list'."
  (interactive)
  (require 'recentf)
  (recentf-mode)
  (ivy-read "Recentf: " (mapcar #'substring-no-properties recentf-list)
            :action (lambda (f)
                      (with-ivy-window
                       (find-file f)))
            :caller 'counsel-recentf))
(ivy-set-actions
 'counsel-recentf
 '(("j" find-file-other-window "other window")
   ("x" counsel-find-file-extern "open externally")))

;;** `counsel-locate'
(defcustom counsel-locate-cmd (cond ((eq system-type 'darwin)
                                     'counsel-locate-cmd-noregex)
                                    ((and (eq system-type 'windows-nt)
                                          (executable-find "es.exe"))
                                     'counsel-locate-cmd-es)
                                    (t
                                     'counsel-locate-cmd-default))
  "The function for producing a locate command string from the input.

The function takes a string - the current input, and returns a
string - the full shell command to run."
  :group 'ivy
  :type '(choice
          (const :tag "Default" counsel-locate-cmd-default)
          (const :tag "No regex" counsel-locate-cmd-noregex)
          (const :tag "mdfind" counsel-locate-cmd-mdfind)
          (const :tag "everything" counsel-locate-cmd-es)))

(ivy-set-actions
 'counsel-locate
 '(("x" counsel-locate-action-extern "xdg-open")
   ("d" counsel-locate-action-dired "dired")))

(counsel-set-async-exit-code 'counsel-locate 1 "Nothing found")

(defvar counsel-locate-history nil
  "History for `counsel-locate'.")

(defun counsel-locate-action-extern (x)
  "Use xdg-open shell command, or corresponding system command, on X."
  (interactive (list (read-file-name "File: ")))
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" x)
    (call-process shell-file-name nil
                  nil nil
                  shell-command-switch
                  (format "%s %s"
                          (cl-case system-type
                            (darwin "open")
                            (t "xdg-open"))
                          (shell-quote-argument x)))))

(defalias 'counsel-find-file-extern 'counsel-locate-action-extern)

(declare-function dired-jump "dired-x")

(defun counsel-locate-action-dired (x)
  "Use `dired-jump' on X."
  (dired-jump nil x))

(defun counsel-locate-cmd-default (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "locate")
  (format "locate -i --regex '%s'"
          (counsel-unquote-regex-parens
           (ivy--regex input))))

(defun counsel-locate-cmd-noregex (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "locate")
  (format "locate -i '%s'" input))

(defun counsel-locate-cmd-mdfind (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "mdfind")
  (format "mdfind -name '%s'" input))

(defun counsel-locate-cmd-es (input)
  "Return a shell command based on INPUT."
  (counsel-require-program "es.exe")
  (format "es.exe -i -r %s"
          (counsel-unquote-regex-parens
           (ivy--regex input t))))

(defun counsel-locate-function (input)
  "Call the \"locate\" shell command with INPUT."
  (if (< (length input) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (funcall counsel-locate-cmd input))
    '("" "working...")))

;;;###autoload
(defun counsel-locate (&optional initial-input)
  "Call the \"locate\" shell command.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "Locate: " #'counsel-locate-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-locate-history
            :action (lambda (file)
                      (with-ivy-window
                        (when file
                          (find-file file))))
            :unwind #'counsel-delete-process
            :caller 'counsel-locate))

;;** `counsel-dpkg'
;;;###autoload
(defun counsel-dpkg ()
  "Call the \"dpkg\" shell command."
  (interactive)
  (counsel-require-program "dpkg")
  (let ((cands (mapcar
                (lambda (x)
                  (let ((y (split-string x "  +")))
                    (cons (format "%-40s   %s"
                                  (ivy--truncate-string
                                   (nth 1 y) 40)
                                  (nth 4 y))
                          (mapconcat #'identity y " "))))
                (split-string
                 (shell-command-to-string "dpkg -l | tail -n+6") "\n" t))))
    (ivy-read "dpkg: " cands
              :action (lambda (x)
                        (message (cdr x)))
              :caller 'counsel-dpkg)))

;;** `counsel-rpm'
;;;###autoload
(defun counsel-rpm ()
  "Call the \"rpm\" shell command."
  (interactive)
  (counsel-require-program "rpm")
  (let ((cands (mapcar
                (lambda (x)
                  (let ((y (split-string x "|")))
                    (cons (format "%-40s   %s"
                                  (ivy--truncate-string
                                   (nth 0 y) 40)
                                  (nth 1 y))
                          (mapconcat #'identity y " "))))
                (split-string
                 (shell-command-to-string "rpm -qa --qf \"%{NAME}|%{SUMMARY}\\n\"") "\n" t))))
    (ivy-read "rpm: " cands
              :action (lambda (x)
                        (message (cdr x)))
              :caller 'counsel-rpm)))

;;** File Jump and Dired Jump

;;;###autoload
(defun counsel-file-jump (&optional initial-input initial-directory)
  "Jump to a file below the current directory.
List all files within the current directory or any of its subdirectories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name "From directory: "))))
  (counsel-require-program "find")
  (let* ((default-directory (or initial-directory default-directory)))
    (ivy-read "Find file: "
              (split-string
               (shell-command-to-string
                (concat find-program " * -type f -not -path '*\/.git*'"))
               "\n" t)
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action (lambda (x)
                        (with-ivy-window
                          (find-file (expand-file-name x ivy--directory))))
              :preselect (when counsel-find-file-at-point
                           (require 'ffap)
                           (let ((f (ffap-guesser)))
                             (when f (expand-file-name f))))
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'counsel-file-jump)))

;;;###autoload
(defun counsel-dired-jump (&optional initial-input initial-directory)
  "Jump to a directory (in dired) below the current directory.
List all subdirectories within the current directory.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name "From directory: "))))
  (counsel-require-program "find")
  (let* ((default-directory (or initial-directory default-directory)))
    (ivy-read "Directory: "
              (split-string
               (shell-command-to-string
                (concat find-program " * -type d -not -path '*\/.git*'"))
               "\n" t)
              :initial-input initial-input
              :action (lambda (d) (dired-jump nil (expand-file-name d)))
              :caller 'counsel-dired-jump)))

;;* Grep
(defun counsel--grep-mode-occur (git-grep-dir-is-file)
  "Generate a custom occur buffer for grep like commands.
If GIT-GREP-DIR-IS-FILE is t, then `counsel--git-dir' is treated as a full
path to a file rather than a directory (e.g. for `counsel-grep-occur').

This function expects that the candidates have already been filtered.
It applies no filtering to ivy--all-candidates."
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode))
  (let* ((directory
          (if git-grep-dir-is-file
              (file-name-directory counsel--git-dir)
            counsel--git-dir))
         (prepend
          (if git-grep-dir-is-file
              (concat (file-name-nondirectory counsel--git-dir) ":")
            "")))
    (setq default-directory directory)
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n" default-directory))
    (insert (format "%d candidates:\n" (length ivy--all-candidates)))
    (ivy--occur-insert-lines
     (mapcar (lambda (cand) (concat "./" prepend cand)) ivy--all-candidates))))

;;** `counsel-ag'
(defvar counsel-ag-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'ivy-call-and-recenter)
    (define-key map (kbd "M-q") 'counsel-git-grep-query-replace)
    map))

(defcustom counsel-ag-base-command
  (if (memq system-type '(ms-dos windows-nt))
      "ag --vimgrep %s"
    "ag --nocolor --nogroup %s")
  "Format string to use in `counsel-ag-function' to construct the command.
The %s will be replaced by optional extra ag arguments followed by the
regex string.  The default is \"ag --nocolor --nogroup %s\"."
  :type 'string
  :group 'ivy)

(counsel-set-async-exit-code 'counsel-ag 1 "No matches found")
(ivy-set-occur 'counsel-ag 'counsel-ag-occur)
(ivy-set-display-transformer 'counsel-ag 'counsel-git-grep-transformer)

(defun counsel-ag-function (string base-cmd extra-ag-args)
  "Grep in the current directory for STRING using BASE-CMD.
If non-nil, append EXTRA-AG-ARGS to BASE-CMD."
  (when (null extra-ag-args)
    (setq extra-ag-args ""))
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (let ((default-directory counsel--git-dir)
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (let* ((args-end (string-match " -- " extra-ag-args))
             (file (if args-end
                       (substring-no-properties extra-ag-args (+ args-end 3))
                     ""))
             (extra-ag-args (if args-end
                                (substring-no-properties extra-ag-args 0 args-end)
                              extra-ag-args))
             (ag-cmd (format base-cmd
                             (concat extra-ag-args
                                     " -- "
                                     (shell-quote-argument regex)
                                     file))))
        (counsel--async-command ag-cmd)
        nil))))

;;;###autoload
(defun counsel-ag (&optional initial-input initial-directory extra-ag-args ag-prompt)
  "Grep for a string in the current directory using ag.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS string, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument."
  (interactive)
  (counsel-require-program (car (split-string counsel-ag-base-command)))
  (when current-prefix-arg
    (setq initial-directory
          (or initial-directory
              (read-directory-name (concat
                                    (car (split-string counsel-ag-base-command))
                                    " in directory: "))))
    (setq extra-ag-args
          (or extra-ag-args
              (let* ((pos (cl-position ?  counsel-ag-base-command))
                     (command (substring-no-properties counsel-ag-base-command 0 pos))
                     (ag-args (replace-regexp-in-string
                               "%s" "" (substring-no-properties counsel-ag-base-command pos))))
                (read-string (format "(%s) args:" command) ag-args)))))
  (ivy-set-prompt 'counsel-ag counsel-prompt-function)
  (setq counsel--git-dir (or initial-directory default-directory))
  (ivy-read (or ag-prompt (car (split-string counsel-ag-base-command)))
            (lambda (string)
              (counsel-ag-function string counsel-ag-base-command extra-ag-args))
            :initial-input initial-input
            :dynamic-collection t
            :keymap counsel-ag-map
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-ag))

(defun counsel-grep-like-occur (cmd-template)
  (unless (eq major-mode 'ivy-occur-grep-mode)
    (ivy-occur-grep-mode)
    (setq default-directory counsel--git-dir))
  (setq ivy-text
        (and (string-match "\"\\(.*\\)\"" (buffer-name))
             (match-string 1 (buffer-name))))
  (let* ((cmd (format cmd-template
                      (counsel-unquote-regex-parens
                       (ivy--regex ivy-text))))
         (cands (split-string (shell-command-to-string cmd) "\n" t)))
    ;; Need precise number of header lines for `wgrep' to work.
    (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                    default-directory))
    (insert (format "%d candidates:\n" (length cands)))
    (ivy--occur-insert-lines
     (mapcar
      (lambda (cand) (concat "./" cand))
      cands))))

(defun counsel-ag-occur ()
  "Generate a custom occur buffer for `counsel-ag'."
  (counsel-grep-like-occur
   "ag --nocolor --nogroup %s"))

;;** `counsel-pt'
(defcustom counsel-pt-base-command "pt --nocolor --nogroup -e %s"
  "Alternative to `counsel-ag-base-command' using pt."
  :type 'string
  :group 'ivy)

;;;###autoload
(defun counsel-pt (&optional initial-input)
  "Grep for a string in the current directory using pt.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-pt-base-command' instead of
`counsel-ag-base-command'."
  (interactive)
  (let ((counsel-ag-base-command counsel-pt-base-command))
    (counsel-ag initial-input)))

;;** `counsel-ack'
(defcustom counsel-ack-base-command
  (concat
   (file-name-nondirectory
    (or (executable-find "ack-grep") "ack"))
   " --nocolor --nogroup %s")
  "Alternative to `counsel-ag-base-command' using ack."
  :type 'string
  :group 'ivy)

;;;###autoload
(defun counsel-ack (&optional initial-input)
  "Grep for a string in the current directory using ack.
INITIAL-INPUT can be given as the initial minibuffer input.
This uses `counsel-ag' with `counsel-ack-base-command' replacing
`counsel-ag-base-command'."
  (interactive)
  (let ((counsel-ag-base-command counsel-ack-base-command))
    (counsel-ag initial-input)))


;;** `counsel-rg'
(defcustom counsel-rg-base-command "rg -i --no-heading --line-number --color never %s ."
  "Alternative to `counsel-ag-base-command' using ripgrep."
  :type 'string
  :group 'ivy)

(counsel-set-async-exit-code 'counsel-rg 1 "No matches found")
(ivy-set-occur 'counsel-rg 'counsel-rg-occur)
(ivy-set-display-transformer 'counsel-rg 'counsel-git-grep-transformer)

;;;###autoload
(defun counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "Grep for a string in the current directory using rg.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name (concat
                                 (car (split-string counsel-rg-base-command))
                                 " in directory: ")))))
  (counsel-require-program (car (split-string counsel-rg-base-command)))
  (ivy-set-prompt 'counsel-rg counsel-prompt-function)
  (setq counsel--git-dir (or initial-directory
                             (locate-dominating-file default-directory ".git")
                             default-directory))
  (ivy-read (or rg-prompt (car (split-string counsel-rg-base-command)))
            (lambda (string)
              (counsel-ag-function string counsel-rg-base-command extra-rg-args))
            :initial-input initial-input
            :dynamic-collection t
            :keymap counsel-ag-map
            :history 'counsel-git-grep-history
            :action #'counsel-git-grep-action
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))
            :caller 'counsel-rg))

(defun counsel-rg-occur ()
  "Generate a custom occur buffer for `counsel-rg'."
  (counsel-grep-like-occur
   "rg -i --no-heading --line-number --color never \"%s\" ."))

;;** `counsel-grep'
(defcustom counsel-grep-base-command "grep -nE '%s' %s"
  "Format string to use in `cousel-grep-function' to construct the command."
  :type 'string
  :group 'ivy)

(defvar counsel-grep-command nil)

(defun counsel-grep-function (string)
  "Grep in the current directory for STRING."
  (if (< (length string) 2)
      (counsel-more-chars 2)
    (let ((regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (counsel--async-command
       (format counsel-grep-command regex))
      nil)))

(defun counsel-grep-action (x)
  "Go to candidate X."
  (with-ivy-window
    (swiper--cleanup)
    (let ((default-directory (file-name-directory counsel--git-dir))
          file-name line-number)
      (when (cond ((string-match "\\`\\([0-9]+\\):\\(.*\\)\\'" x)
                   (setq file-name (buffer-file-name (ivy-state-buffer ivy-last)))
                   (setq line-number (match-string-no-properties 1 x)))
                  ((string-match "\\`\\([^:]+\\):\\([0-9]+\\):\\(.*\\)\\'" x)
                   (setq file-name (match-string-no-properties 1 x))
                   (setq line-number (match-string-no-properties 2 x)))
                  (t nil))
        ;; If the file buffer is already open, just get it. Prevent doing
        ;; `find-file', as that file could have already been opened using
        ;; `find-file-literally'.
        (let ((buf (get-file-buffer file-name)))
          (unless buf
            (setq buf (find-file file-name)))
          (with-current-buffer buf
            (setq line-number (string-to-number line-number))
            (if (null counsel-grep-last-line)
                (progn
                  (goto-char (point-min))
                  (forward-line (1- (setq counsel-grep-last-line line-number))))
              (forward-line (- line-number counsel-grep-last-line))
              (setq counsel-grep-last-line line-number))
            (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
            (run-hooks 'counsel-grep-post-action-hook)
            (if (eq ivy-exit 'done)
                (swiper--ensure-visible)
              (isearch-range-invisible (line-beginning-position)
                                       (line-end-position))
              (swiper--add-overlays (ivy--regex ivy-text)))))))))

(defun counsel-grep-occur ()
  "Generate a custom occur buffer for `counsel-grep'."
  (counsel-grep-like-occur
   (format
    "grep -niE '%%s' %s /dev/null"
    (shell-quote-argument
     (file-name-nondirectory
      (buffer-file-name
       (ivy-state-buffer ivy-last)))))))

(ivy-set-occur 'counsel-grep 'counsel-grep-occur)
(counsel-set-async-exit-code 'counsel-grep 1 "")

;;;###autoload
(defun counsel-grep ()
  "Grep for a string in the current file."
  (interactive)
  (counsel-require-program (car (split-string counsel-grep-base-command)))
  (setq counsel-grep-last-line nil)
  (setq counsel--git-dir default-directory)
  (if (string-match "%s$" counsel-grep-base-command)
      (setq counsel-grep-command
            (concat (substring counsel-grep-base-command 0 (match-beginning 0))
                    (shell-quote-argument (buffer-file-name))))
    (error "expected `counsel-grep-base-command' to end in %%s"))
  (let ((init-point (point))
        res)
    (unwind-protect
         (setq res (ivy-read "grep: " 'counsel-grep-function
                             :dynamic-collection t
                             :preselect (format "%d:%s"
                                                (line-number-at-pos)
                                                (regexp-quote
                                                 (buffer-substring-no-properties
                                                  (line-beginning-position)
                                                  (line-end-position))))
                             :history 'counsel-git-grep-history
                             :update-fn (lambda ()
                                          (counsel-grep-action (ivy-state-current ivy-last)))
                             :re-builder #'ivy--regex
                             :action #'counsel-grep-action
                             :unwind (lambda ()
                                       (counsel-delete-process)
                                       (swiper--cleanup))
                             :caller 'counsel-grep))
      (unless res
        (goto-char init-point)))))

;;** `counsel-grep-or-swiper'
(defcustom counsel-grep-swiper-limit 300000
  "When the buffer is larger than this, use `counsel-grep' instead of `swiper'."
  :type 'integer
  :group 'ivy)

(defvar counsel-compressed-file-regex
  (progn
    (require 'jka-compr nil t)
    (jka-compr-build-file-regexp))
  "Store the regex for compressed file names.")

;;;###autoload
(defun counsel-grep-or-swiper ()
  "Call `swiper' for small buffers and `counsel-grep' for large ones."
  (interactive)
  (let ((fname (buffer-file-name)))
    (if (and fname
             (not (buffer-narrowed-p))
             (not (ignore-errors
                    (file-remote-p fname)))
             (not (string-match
                   counsel-compressed-file-regex
                   fname))
             (> (buffer-size)
                (if (eq major-mode 'org-mode)
                    (/ counsel-grep-swiper-limit 4)
                  counsel-grep-swiper-limit)))
        (progn
          (when (file-writable-p fname)
            (save-buffer))
          (counsel-grep))
      (swiper--ivy (swiper--candidates)))))

;;** `counsel-recoll'
(defun counsel-recoll-function (string)
  "Run recoll for STRING."
  (if (< (length string) 3)
      (counsel-more-chars 3)
    (counsel--async-command
     (format "recoll -t -b %s"
             (shell-quote-argument string)))
    nil))

;; This command uses the recollq command line tool that comes together
;; with the recoll (the document indexing database) source:
;;     http://www.lesbonscomptes.com/recoll/download.html
;; You need to build it yourself (together with recoll):
;;     cd ./query && make && sudo cp recollq /usr/local/bin
;; You can try the GUI version of recoll with:
;;     sudo apt-get install recoll
;; Unfortunately, that does not install recollq.
(defun counsel-recoll (&optional initial-input)
  "Search for a string in the recoll database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (counsel-require-program "recoll")
  (ivy-read "recoll: " 'counsel-recoll-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :action (lambda (x)
                      (when (string-match "file://\\(.*\\)\\'" x)
                        (let ((file-name (match-string 1 x)))
                          (find-file file-name)
                          (unless (string-match "pdf$" x)
                            (swiper ivy-text)))))
            :unwind #'counsel-delete-process
            :caller 'counsel-recoll))
;;* Misc Emacs
;;** `counsel-org-tag'
(defvar counsel-org-tags nil
  "Store the current list of tags.")

(defvar org-outline-regexp)
(defvar org-indent-mode)
(defvar org-indent-indentation-per-level)
(defvar org-tags-column)
(declare-function org-get-tags-string "org")
(declare-function org-move-to-column "org-compat")

(defun counsel-org-change-tags (tags)
  "Change tags of current org headline to TAGS."
  (let ((current (org-get-tags-string))
        (col (current-column))
        level)
    ;; Insert new tags at the correct column
    (beginning-of-line 1)
    (setq level (or (and (looking-at org-outline-regexp)
                         (- (match-end 0) (point) 1))
                    1))
    (cond
      ((and (equal current "") (equal tags "")))
      ((re-search-forward
        (concat "\\([ \t]*" (regexp-quote current) "\\)[ \t]*$")
        (point-at-eol) t)
       (if (equal tags "")
           (delete-region
            (match-beginning 0)
            (match-end 0))
         (goto-char (match-beginning 0))
         (let* ((c0 (current-column))
                ;; compute offset for the case of org-indent-mode active
                (di (if (bound-and-true-p org-indent-mode)
                        (* (1- org-indent-indentation-per-level) (1- level))
                      0))
                (p0 (if (equal (char-before) ?*) (1+ (point)) (point)))
                (tc (+ org-tags-column (if (> org-tags-column 0) (- di) di)))
                (c1 (max (1+ c0) (if (> tc 0) tc (- (- tc) (string-width tags)))))
                (rpl (concat (make-string (max 0 (- c1 c0)) ?\ ) tags)))
           (replace-match rpl t t)
           (and c0 indent-tabs-mode (tabify p0 (point)))
           tags)))
      (t (error "Tags alignment failed")))
    (org-move-to-column col)))

(defun counsel-org--set-tags ()
  "Set tags of current org headline to `counsel-org-tags'."
  (counsel-org-change-tags
   (if counsel-org-tags
       (format ":%s:"
               (mapconcat #'identity counsel-org-tags ":"))
     "")))

(defvar org-agenda-bulk-marked-entries)

(declare-function org-get-at-bol "org")
(declare-function org-agenda-error "org-agenda")

(defun counsel-org-tag-action (x)
  "Add tag X to `counsel-org-tags'.
If X is already part of the list, remove it instead.  Quit the selection if
X is selected by either `ivy-done', `ivy-alt-done' or `ivy-immediate-done',
otherwise continue prompting for tags."
  (if (member x counsel-org-tags)
      (progn
        (setq counsel-org-tags (delete x counsel-org-tags)))
    (unless (equal x "")
      (setq counsel-org-tags (append counsel-org-tags (list x)))
      (unless (member x ivy--all-candidates)
        (setq ivy--all-candidates (append ivy--all-candidates (list x))))))
  (let ((prompt (counsel-org-tag-prompt)))
    (setf (ivy-state-prompt ivy-last) prompt)
    (setq ivy--prompt (concat "%-4d " prompt)))
  (cond ((memq this-command '(ivy-done
                              ivy-alt-done
                              ivy-immediate-done))
         (if (eq major-mode 'org-agenda-mode)
             (if (null org-agenda-bulk-marked-entries)
                 (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                                     (org-agenda-error))))
                   (with-current-buffer (marker-buffer hdmarker)
                     (goto-char hdmarker)
                     (counsel-org--set-tags)))
               (let ((add-tags (copy-sequence counsel-org-tags)))
                 (dolist (m org-agenda-bulk-marked-entries)
                   (with-current-buffer (marker-buffer m)
                     (save-excursion
                       (goto-char m)
                       (setq counsel-org-tags
                             (delete-dups
                              (append (split-string (org-get-tags-string) ":" t)
                                      add-tags)))
                       (counsel-org--set-tags))))))
           (counsel-org--set-tags)))
        ((eq this-command 'ivy-call)
         (with-selected-window (active-minibuffer-window)
           (delete-minibuffer-contents)))))

(defun counsel-org-tag-prompt ()
  "Return prompt for `counsel-org-tag'."
  (format "Tags (%s): "
          (mapconcat #'identity counsel-org-tags ", ")))

(defvar org-setting-tags)
(defvar org-last-tags-completion-table)
(defvar org-tag-persistent-alist)
(defvar org-tag-alist)
(defvar org-complete-tags-always-offer-all-agenda-tags)

(declare-function org-at-heading-p "org")
(declare-function org-back-to-heading "org")
(declare-function org-get-buffer-tags "org")
(declare-function org-global-tags-completion-table "org")
(declare-function org-agenda-files "org")
(declare-function org-agenda-set-tags "org-agenda")

;;;###autoload
(defun counsel-org-tag ()
  "Add or remove tags in `org-mode'."
  (interactive)
  (save-excursion
    (if (eq major-mode 'org-agenda-mode)
        (if org-agenda-bulk-marked-entries
            (setq counsel-org-tags nil)
          (let ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                              (org-agenda-error))))
            (with-current-buffer (marker-buffer hdmarker)
              (goto-char hdmarker)
              (setq counsel-org-tags
                    (split-string (org-get-tags-string) ":" t)))))
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      (setq counsel-org-tags (split-string (org-get-tags-string) ":" t)))
    (let ((org-setting-tags t)
          (org-last-tags-completion-table
           (append org-tag-persistent-alist
                   (or org-tag-alist (org-get-buffer-tags))
                   (and
                    (or org-complete-tags-always-offer-all-agenda-tags
                        (eq major-mode 'org-agenda-mode))
                    (org-global-tags-completion-table
                     (org-agenda-files))))))
      (ivy-read (counsel-org-tag-prompt)
                (lambda (str &rest _unused)
                  (delete-dups
                   (all-completions str 'org-tags-completion-function)))
                :history 'org-tags-history
                :action 'counsel-org-tag-action
                :caller 'counsel-org-tag))))

;;;###autoload
(defun counsel-org-tag-agenda ()
  "Set tags for the current agenda item."
  (interactive)
  (let ((store (symbol-function 'org-set-tags)))
    (unwind-protect
         (progn
           (fset 'org-set-tags
                 (symbol-function 'counsel-org-tag))
           (org-agenda-set-tags nil nil))
      (fset 'org-set-tags store))))

(defcustom counsel-org-goto-display-style 'path
  "The style for displaying headlines in `counsel-org-goto' functions.

If headline, the title and the leading stars are displayed.

If path, the path hierarchy is displayed.  For each entry the title is shown.
`counsel-org-goto-separator' is used as separator between entries.

If title or any other value, only the title of the headline is displayed.

Use `counsel-org-goto-display-tags' and `counsel-org-goto-display-todo' to
display tags and todo keywords respectively."
  :type '(choice
          (const :tag "Title only" title)
          (const :tag "Headline" headline)
          (const :tag "Path" path))
  :group 'ivy)

(defcustom counsel-org-goto-separator "/"
  "Character(s) to separate path entries in `counsel-org-goto' functions.
This variable has no effect unless `counsel-org-goto-display-style' is
set to path."
  :type 'string
  :group 'ivy)

(defcustom counsel-org-goto-display-tags nil
  "If non-nil, display tags in `counsel-org-goto' functions."
  :type 'boolean
  :group 'ivy)

(defcustom counsel-org-goto-display-todo nil
  "If non-nil, display todo keywords in `counsel-org-goto' functions."
  :type 'boolean
  :group 'ivy)

(defcustom counsel-org-goto-face-style nil
  "The face used for displaying headlines in `counsel-org-goto' functions.

If org, the default faces from `org-mode' are applied, i.e. org-level-1
through org-level-8.  Note that no cycling is in effect, therefore headlines
on levels 9 and higher will not be styled.

If verbatim, the face used in the buffer is applied.  For simple headlines
this is usually the same as org except that it depends on how much of the
buffer has been completely loaded.  If your buffer exceeds a certain size,
headlines are styled lazily depending on which parts of the tree are visible.
Headlines which are not styled yet in the buffer will appear unstyled in the
minibuffer as well.  If your headlines contain parts which are fontified
differently than the headline itself (eg. todo keywords, tags, links) and you
want these parts to be styled properly, verbatim is the way to go, otherwise
you are probably better off using org instead.

If custom, the faces defined in `counsel-org-goto-custom-faces' are applied.
Note that no cycling is in effect, therefore if there is no face defined
for a certain level, headlines on that level will not be styled.

If nil or any other value, no face is applied to the headline.

See `counsel-org-goto-display-tags' and `counsel-org-goto-display-todo' if
you want to display tags and todo keywords in your headlines."
  :type '(choice
          (const :tag "Same as org-mode" org)
          (const :tag "Verbatim" verbatim)
          (const :tag "Custom" custom))
  :group 'ivy)

(defcustom counsel-org-goto-custom-faces nil
  "Custom faces for displaying headlines in `counsel-org-goto' functions.

The n-th entry is used for headlines on level n, starting with n = 1.  If
a headline is an a level for which there is no entry in the list, it will
not be styled.

This variable has no effect unless `counsel-org-goto-face-style' is set
to custom."
  :type '(repeat face)
  :group 'ivy)

(declare-function org-get-heading "org")
(declare-function org-goto-marker-or-bmk "org")
(declare-function outline-next-heading "outline")

;;;###autoload
(defun counsel-org-goto ()
  "Go to a different location in the current file."
  (interactive)
  (let ((entries (counsel-org-goto--get-headlines)))
    (ivy-read "Goto: "
              entries
              :history 'counsel-org-goto-history
              :action 'counsel-org-goto-action
              :caller 'counsel-org-goto)))

;;;###autoload
(defun counsel-org-goto-all ()
  "Go to a different location in any org file."
  (interactive)
  (let (entries)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (derived-mode-p 'org-mode)
          (if entries
              (nconc entries (counsel-org-goto--get-headlines))
            (setq entries (counsel-org-goto--get-headlines))))))
    (ivy-read "Goto: "
              entries
              :history 'counsel-org-goto-history
              :action 'counsel-org-goto-action
              :caller 'counsel-org-goto-all)))

(defun counsel-org-goto-action (x)
  "Go to headline in candidate X."
  (org-goto-marker-or-bmk (cdr x)))

(defun counsel-org-goto--get-headlines ()
  "Get all headlines from the current org buffer."
  (save-excursion
    (let (entries
          start-pos
          stack
          (stack-level 0))
      (goto-char (point-min))
      (setq start-pos (or (and (org-at-heading-p)
                               (point))
                          (outline-next-heading)))
      (while start-pos
        (let ((name (org-get-heading
                     (not counsel-org-goto-display-tags)
                     (not counsel-org-goto-display-todo)))
              level)
          (search-forward " ")
          (setq level
                (- (length (buffer-substring-no-properties start-pos (point)))
                   1))
          (cond ((eq counsel-org-goto-display-style 'path)
                 ;; Update stack. The empty entry guards against incorrect
                 ;; headline hierarchies e.g. a level 3 headline immediately
                 ;; following a level 1 entry.
                 (while (<= level stack-level)
                   (pop stack)
                   (cl-decf stack-level))
                 (while (> level stack-level)
                   (push "" stack)
                   (cl-incf stack-level))
                 (setf (car stack) (counsel-org-goto--add-face name level))
                 (setq name (mapconcat
                             #'identity
                             (reverse stack)
                             counsel-org-goto-separator)))
                (t
                 (when (eq counsel-org-goto-display-style 'headline)
                   (setq name (concat (make-string level ?*) " " name)))
                 (setq name (counsel-org-goto--add-face name level))))
          (push (cons name (point-marker)) entries))
        (setq start-pos (outline-next-heading)))
      (nreverse entries))))

(defun counsel-org-goto--add-face (name level)
  "Add face to headline NAME on LEVEL.
The face can be customized through `counsel-org-goto-face-style'."
  (or (and (eq counsel-org-goto-face-style 'org)
           (propertize
            name
            'face
            (concat "org-level-" (number-to-string level))))
      (and (eq counsel-org-goto-face-style 'verbatim)
           name)
      (and (eq counsel-org-goto-face-style 'custom)
           (propertize
            name
            'face
            (nth (1- level) counsel-org-goto-custom-faces)))
      (propertize name 'face 'minibuffer-prompt)))

;;** `counsel-org-file'
(defun counsel-org-file-ids ()
  (let (cands)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^:ID: *\\([^ \n]+\\)$" nil t)
        (push (match-string-no-properties 1) cands)))
    (nreverse cands)))

(defun counsel-org-files ()
  (mapcar 'file-relative-name
          (cl-mapcan
           (lambda (id)
             (directory-files
              (format "data/%s/%s"
                      (substring id 0 2)
                      (substring id 2))
              t "^[^.]"))
           (counsel-org-file-ids))))

;;;###autoload
(defun counsel-org-file ()
  "Browse all attachments for current Org file."
  (interactive)
  (ivy-read "file: " (counsel-org-files)
            :action 'counsel-locate-action-dired
            :caller 'counsel-org-file))

;;** `counsel-mark-ring'
(defun counsel--pad (string length)
  "Pad STRING to LENGTH with spaces."
  (let ((padding (max 0 (- length (length string)))))
    (concat string (make-string padding ?\s))))

(defun counsel-mark-ring ()
  "Browse `mark-ring' interactively."
  (interactive)
  (let ((candidates
         (with-current-buffer (current-buffer)
           (let ((padding (length (format "%s: " (line-number-at-pos (eobp))))))
             (save-mark-and-excursion
              (goto-char (point-min))
              (sort (mapcar (lambda (mark)
                              (let* ((position (marker-position mark))
                                     (line-number (line-number-at-pos position))
                                     (line-marker (counsel--pad (format "%s:" line-number) padding))
                                     (bol (point-at-bol line-number))
                                     (eol (point-at-eol line-number))
                                     (line (buffer-substring bol eol)))
                                (cons (format "%s%s" line-marker line) position)))
                            (cl-remove-duplicates mark-ring :test #'equal))
                    (lambda (m1 m2)
                      (< (cdr m1) (cdr m2)))))))))
    (ivy-read "Marks: " candidates
              :action (lambda (elem)
                        (goto-char (cdr elem))))))

;;** `counsel-package'
(defvar package--initialized)
(defvar package-alist)
(defvar package-archive-contents)
(declare-function package-installed-p "package")
(declare-function package-delete "package")

(defun counsel-package ()
  "Install or delete packages.

Packages not currently installed have a \"+\" prepended.  Selecting one
of these will try to install it.  Currently installed packages have a
\"-\" prepended, and selecting one of these will delete the package.

Additional Actions:

  \\<ivy-minibuffer-map>\\[ivy-dispatching-done] d: describe package"
  (interactive)
  (unless package--initialized
    (package-initialize t))
  (unless package-archive-contents
    (package-refresh-contents))
  (let ((cands (mapcar #'counsel-package-make-package-cell
                       package-archive-contents)))
    (ivy-read "Packages (install +pkg or delete -pkg): "
              (cl-sort cands #'counsel--package-sort)
              :action #'counsel-package-action
              :initial-input "^+ "
              :require-match t
              :caller 'counsel-package)))

(defun counsel-package-make-package-cell (pkg)
  "Make candidate for package PKG."
  (let* ((pkg-sym (car pkg))
         (pkg-name (symbol-name pkg-sym)))
    (cons (format "%s%s"
                  (if (package-installed-p pkg-sym) "-" "+")
                  pkg-name)
          pkg)))

(defun counsel-package-action (pkg-cons)
  "Delete or install package in PKG-CONS."
  (let ((pkg (cadr pkg-cons)))
    (if (package-installed-p pkg)
        (package-delete
         (cadr (assoc pkg package-alist)))
      (package-install pkg))))

(defun counsel-package-action-describe (pkg-cons)
  "Call `describe-package' for package in PKG-CONS."
  (describe-package (cadr pkg-cons)))

(declare-function package-desc-extras "package")

(defun counsel-package-action-homepage (pkg-cons)
  "Open homepage for package in PKG-CONS."
  (let* ((desc-list (cddr pkg-cons))
         (desc (if (listp desc-list) (car desc-list) desc-list))
         (url (cdr (assoc :url (package-desc-extras desc)))))
    (when url
      (require 'browse-url)
      (browse-url url))))

(defun counsel--package-sort (a b)
  "Sort function for `counsel-package'.
A is the left hand side, B the right hand side."
  (let* ((a (car a))
         (b (car b))
         (a-inst (equal (substring a 0 1) "+"))
         (b-inst (equal (substring b 0 1) "+")))
    (or (and a-inst (not b-inst))
        (and (eq a-inst b-inst) (string-lessp a b)))))

(ivy-set-actions 'counsel-package
                 '(("d" counsel-package-action-describe "describe package")
                   ("h" counsel-package-action-homepage "open package homepage")))

;;** `counsel-tmm'
(defvar tmm-km-list nil)
(declare-function tmm-get-keymap "tmm")
(declare-function tmm--completion-table "tmm")
(declare-function tmm-get-keybind "tmm")

(defun counsel-tmm-prompt (menu)
  "Select and call an item from the MENU keymap."
  (let (out
        choice
        chosen-string)
    (setq tmm-km-list nil)
    (map-keymap (lambda (k v) (tmm-get-keymap (cons k v))) menu)
    (setq tmm-km-list (nreverse tmm-km-list))
    (setq out (ivy-read "Menu bar: " (tmm--completion-table tmm-km-list)
                        :require-match t
                        :sort nil))
    (setq choice (cdr (assoc out tmm-km-list)))
    (setq chosen-string (car choice))
    (setq choice (cdr choice))
    (cond ((keymapp choice)
           (counsel-tmm-prompt choice))
          ((and choice chosen-string)
           (setq last-command-event chosen-string)
           (call-interactively choice)))))

(defvar tmm-table-undef)

;;;###autoload
(defun counsel-tmm ()
  "Text-mode emulation of looking and choosing from a menubar."
  (interactive)
  (require 'tmm)
  (run-hooks 'menu-bar-update-hook)
  (setq tmm-table-undef nil)
  (counsel-tmm-prompt (tmm-get-keybind [menu-bar])))

;;** `counsel-yank-pop'
(defun counsel--yank-pop-truncate (str)
  "Truncate STR for use in `counsel-yank-pop'."
  (condition-case nil
      (let* ((lines (split-string str "\n" t))
             (n (length lines))
             (re (ivy-re-to-str ivy--old-re))
             (first-match (cl-position-if
                           (lambda (s) (string-match re s))
                           lines))
             (beg (max 0 (- first-match
                            counsel-yank-pop-truncate-radius)))
             (end (min n (+ first-match
                            counsel-yank-pop-truncate-radius
                            1)))
             (seq (cl-subseq lines beg end)))
        (if (null first-match)
            (error "Could not match %s" str)
          (when (> beg 0)
            (setcar seq (concat "[...] " (car seq))))
          (when (< end n)
            (setcar (last seq)
                    (concat (car (last seq)) " [...]")))
          (mapconcat #'identity seq "\n")))
    (error str)))

(defcustom counsel-yank-pop-separator "\n"
  "Separator for the kill ring strings in `counsel-yank-pop'."
  :group 'ivy
  :type 'string)

(defun counsel--yank-pop-format-function (cand-pairs)
  "Transform CAND-PAIRS into a string for `counsel-yank-pop'."
  (ivy--format-function-generic
   (lambda (str)
     (mapconcat
      (lambda (s)
        (ivy--add-face s 'ivy-current-match))
      (split-string
       (counsel--yank-pop-truncate str) "\n" t)
      "\n"))
   (lambda (str)
     (counsel--yank-pop-truncate str))
   cand-pairs
   counsel-yank-pop-separator))

(defun counsel-yank-pop-action (s)
  "Insert S into the buffer, overwriting the previous yank."
  (with-ivy-window
    (delete-region ivy-completion-beg
                   ivy-completion-end)
    (insert (substring-no-properties s))
    (setq ivy-completion-end (point))))

(defun counsel-yank-pop-action-remove (s)
  "Remove S from the kill ring."
  (setq kill-ring (delete s kill-ring)))

;;;###autoload
(defun counsel-yank-pop ()
  "Ivy replacement for `yank-pop'."
  (interactive)
  (if (eq last-command 'yank)
      (progn
        (setq ivy-completion-end (point))
        (setq ivy-completion-beg
              (save-excursion
                (search-backward (car kill-ring))
                (point))))
    (setq ivy-completion-beg (point))
    (setq ivy-completion-end (point)))
  (let ((candidates
         (mapcar #'ivy-cleanup-string
                 (cl-remove-if
                  (lambda (s)
                    (string-match "\\`[\n[:blank:]]*\\'" s))
                  (delete-dups kill-ring)))))
    (let ((ivy-format-function #'counsel--yank-pop-format-function)
          (ivy-height 5))
      (ivy-read "kill-ring: " candidates
                :action 'counsel-yank-pop-action
                :caller 'counsel-yank-pop))))

(ivy-set-actions
 'counsel-yank-pop
 '(("d" counsel-yank-pop-action-remove "delete")))

;;** `counsel-imenu'
(defvar imenu-auto-rescan)
(defvar imenu-auto-rescan-maxout)
(declare-function imenu--subalist-p "imenu")
(declare-function imenu--make-index-alist "imenu")

(defun counsel-imenu-get-candidates-from (alist &optional prefix)
  "Create a list of (key . value) from ALIST.
PREFIX is used to create the key."
  (cl-mapcan (lambda (elm)
               (if (imenu--subalist-p elm)
                   (counsel-imenu-get-candidates-from
                    (cl-loop for (e . v) in (cdr elm) collect
                         (cons e (if (integerp v) (copy-marker v) v)))
                    ;; pass the prefix to next recursive call
                    (concat prefix (if prefix ".") (car elm)))
                 (let ((key (concat
                             (when prefix
                               (concat
                                (propertize prefix 'face 'compilation-info)
                                ": "))
                             (car elm))))
                   (list (cons key
                               ;; create a imenu candidate here
                               (cons key (if (overlayp (cdr elm))
                                             (overlay-start (cdr elm))
                                           (cdr elm))))))))
             alist))

(defvar counsel-imenu-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-l") 'ivy-call-and-recenter)
    map))

;;;###autoload
(defun counsel-imenu ()
  "Jump to a buffer position indexed by imenu."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (let* ((imenu-auto-rescan t)
         (imenu-auto-rescan-maxout (if current-prefix-arg
                                       (buffer-size)
                                     imenu-auto-rescan-maxout))
         (items (imenu--make-index-alist t))
         (items (delete (assoc "*Rescan*" items) items)))
    (ivy-read "imenu items:" (counsel-imenu-get-candidates-from items)
              :preselect (thing-at-point 'symbol)
              :require-match t
              :action (lambda (candidate)
                        (with-ivy-window
                          ;; In org-mode, (imenu candidate) will expand child node
                          ;; after jump to the candidate position
                          (imenu (cdr candidate))))
              :keymap counsel-imenu-map
              :caller 'counsel-imenu)))

;;** `counsel-list-processes'
(defun counsel-list-processes-action-delete (x)
  "Delete process X."
  (delete-process x)
  (setf (ivy-state-collection ivy-last)
        (setq ivy--all-candidates
              (delete x ivy--all-candidates))))

(defun counsel-list-processes-action-switch (x)
  "Switch to buffer of process X."
  (let* ((proc (get-process x))
         (buf (and proc (process-buffer proc))))
    (if buf
        (switch-to-buffer buf)
      (message "Process %s doesn't have a buffer" x))))

;;;###autoload
(defun counsel-list-processes ()
  "Offer completion for `process-list'.
The default action deletes the selected process.
An extra action allows to switch to the process buffer."
  (interactive)
  (list-processes--refresh)
  (ivy-read "Process: " (mapcar #'process-name (process-list))
            :require-match t
            :action
            '(1
              ("o" counsel-list-processes-action-delete "kill")
              ("s" counsel-list-processes-action-switch "switch"))
            :caller 'counsel-list-processes))

;;** `counsel-ace-link'
(defun counsel-ace-link ()
  "Use Ivy completion for `ace-link'."
  (interactive)
  (let (collection action)
    (cond ((eq major-mode 'Info-mode)
           (setq collection 'ace-link--info-collect)
           (setq action 'ace-link--info-action))
          ((eq major-mode 'help-mode)
           (setq collection 'ace-link--help-collect)
           (setq action 'ace-link--help-action))
          ((eq major-mode 'woman-mode)
           (setq collection 'ace-link--woman-collect)
           (setq action 'ace-link--woman-action))
          ((eq major-mode 'eww-mode)
           (setq collection 'ace-link--eww-collect)
           (setq action 'ace-link--eww-action))
          ((eq major-mode 'compilation-mode)
           (setq collection 'ace-link--eww-collect)
           (setq action 'ace-link--compilation-action))
          ((eq major-mode 'org-mode)
           (setq collection 'ace-link--org-collect)
           (setq action 'ace-link--org-action)))
    (if (null collection)
        (error "%S is not supported" major-mode)
      (ivy-read "Ace-Link: " (funcall collection)
                :action (lambda (x) (funcall action (cdr x)))
                :require-match t
                :caller 'counsel-ace-link))))
;;** `counsel-expression-history'
;;;###autoload
(defun counsel-expression-history ()
  "Select an element of `read-expression-history'.
And insert it into the minibuffer.  Useful during `eval-expression'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (ivy-read "Expr: " (delete-dups read-expression-history)
              :action #'insert)))

;;** `counsel-shell-command-history'
;;;###autoload
(defun counsel-shell-command-history ()
  "Browse shell command history."
  (interactive)
  (ivy-read "cmd: " shell-command-history
            :action #'insert
            :caller 'counsel-shell-command-history))

;;** `counsel-esh-history'
(defun counsel--browse-history (elements)
  "Use Ivy to navigate through ELEMENTS."
  (setq ivy-completion-beg (point))
  (setq ivy-completion-end (point))
  (ivy-read "Symbol name: "
            (delete-dups
             (when (> (ring-size elements) 0)
               (ring-elements elements)))
            :action #'ivy-completion-in-region-action))

(defvar eshell-history-ring)

;;;###autoload
(defun counsel-esh-history ()
  "Browse Eshell history."
  (interactive)
  (require 'em-hist)
  (counsel--browse-history eshell-history-ring))

(defvar comint-input-ring)

;;;###autoload
(defun counsel-shell-history ()
  "Browse shell history."
  (interactive)
  (require 'comint)
  (counsel--browse-history comint-input-ring))

;;** `counsel-hydra-heads'
(defvar hydra-curr-body-fn)
(declare-function hydra-keyboard-quit "ext:hydra")

(defun counsel-hydra-heads ()
  "Call a head of the current/last hydra."
  (interactive)
  (let* ((base (substring
                (prin1-to-string hydra-curr-body-fn)
                0 -4))
         (heads (eval (intern (concat base "heads"))))
         (keymap (eval (intern (concat base "keymap"))))
         (head-names
          (mapcar (lambda (x)
                    (cons
                     (if (nth 2 x)
                         (format "[%s] %S (%s)" (nth 0 x) (nth 1 x) (nth 2 x))
                       (format "[%s] %S" (nth 0 x) (nth 1 x)))
                     (lookup-key keymap (kbd (nth 0 x)))))
                  heads)))
    (ivy-read "head: " head-names
              :action (lambda (x) (call-interactively (cdr x))))
    (hydra-keyboard-quit)))
;;** `counsel-semantic'
(declare-function semantic-tag-start "tag")
(declare-function semantic-tag-of-class-p "tag")
(declare-function semantic-fetch-tags "semantic")

(defun counsel-semantic-action (tag)
  "Got to semantic TAG."
  (with-ivy-window
    (goto-char (semantic-tag-start tag))))

(defun counsel-semantic ()
  "Jump to a semantic tag in the current buffer."
  (interactive)
  (let ((tags
         (mapcar
          (lambda (tag)
            (if (semantic-tag-of-class-p tag 'function)
                (cons
                 (propertize
                  (car tag)
                  'face 'font-lock-function-name-face)
                 (cdr tag))
              tag))
          (semantic-fetch-tags))))
    (ivy-read "tag: " tags
              :action 'counsel-semantic-action)))

;;** `counsel-outline'
(defun counsel-outline-candidates ()
  "Return outline candidates."
  (let (cands)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward outline-regexp nil t)
        (skip-chars-forward " ")
        (push (cons (buffer-substring-no-properties
                     (point) (line-end-position))
                    (line-beginning-position))
              cands))
      (nreverse cands))))

(defun counsel-outline-action (x)
  "Go to outline X."
  (with-ivy-window
    (goto-char (cdr x))))

(defun counsel-outline ()
  "Jump to outline with completion."
  (interactive)
  (ivy-read "outline: " (counsel-outline-candidates)
            :action #'counsel-outline-action))

;;* Misc OS
;;** `counsel-rhythmbox'
(declare-function dbus-call-method "dbus")
(declare-function dbus-get-property "dbus")

(defun counsel-rhythmbox-play-song (song)
  "Let Rhythmbox play SONG."
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/mpris/MediaPlayer2")
        (interface "org.mpris.MediaPlayer2.Player"))
    (dbus-call-method :session service path interface
                      "OpenUri" (cdr song))))

(defun counsel-rhythmbox-enqueue-song (song)
  "Let Rhythmbox enqueue SONG."
  (let ((service "org.gnome.Rhythmbox3")
        (path "/org/gnome/Rhythmbox3/PlayQueue")
        (interface "org.gnome.Rhythmbox3.PlayQueue"))
    (dbus-call-method :session service path interface
                      "AddToQueue" (cdr song))))

(defvar counsel-rhythmbox-history nil
  "History for `counsel-rhythmbox'.")

(defvar counsel-rhythmbox-songs nil)

(defun counsel-rhythmbox-current-song ()
  "Return the currently playing song title."
  (ignore-errors
    (let* ((entry (dbus-get-property
                   :session
                   "org.mpris.MediaPlayer2.rhythmbox"
                   "/org/mpris/MediaPlayer2"
                   "org.mpris.MediaPlayer2.Player"
                   "Metadata"))
           (artist (caar (cadr (assoc "xesam:artist" entry))))
           (album (cl-caadr (assoc "xesam:album" entry)))
           (title (cl-caadr (assoc "xesam:title" entry))))
      (format "%s - %s - %s" artist album title))))

;;;###autoload
(defun counsel-rhythmbox ()
  "Choose a song from the Rhythmbox library to play or enqueue."
  (interactive)
  (require 'dbus)
  (unless counsel-rhythmbox-songs
    (let* ((service "org.gnome.Rhythmbox3")
           (path "/org/gnome/UPnP/MediaServer2/Library/all")
           (interface "org.gnome.UPnP.MediaContainer2")
           (nb-songs (dbus-get-property
                      :session service path interface "ChildCount")))
      (if (not nb-songs)
          (error "Couldn't connect to Rhythmbox")
        (setq counsel-rhythmbox-songs
              (mapcar (lambda (x)
                        (cons
                         (format
                          "%s - %s - %s"
                          (cl-caadr (assoc "Artist" x))
                          (cl-caadr (assoc "Album" x))
                          (cl-caadr (assoc "DisplayName" x)))
                         (cl-caaadr (assoc "URLs" x))))
                      (dbus-call-method
                       :session service path interface "ListChildren"
                       0 nb-songs '("*")))))))
  (ivy-read "Rhythmbox: " counsel-rhythmbox-songs
            :history 'counsel-rhythmbox-history
            :preselect (counsel-rhythmbox-current-song)
            :action
            '(1
              ("p" counsel-rhythmbox-play-song "Play song")
              ("e" counsel-rhythmbox-enqueue-song "Enqueue song"))
            :caller 'counsel-rhythmbox))

;;** `counsel-linux-app'
(defcustom counsel-linux-apps-directories
  '("/usr/local/share/applications/" "/usr/share/applications/")
  "Directories in which to search for applications (.desktop files)."
  :group 'ivy
  :type '(list directory))

(defcustom counsel-linux-app-format-function 'counsel-linux-app-format-function-default
  "Function to format Linux application names the `counsel-linux-app' menu.
The format function will be passed the application's name, comment, and command
as arguments."
  :group 'ivy
  :type '(choice
          (const :tag "Command : Name - Comment" counsel-linux-app-format-function-default)
          (const :tag "Name - Comment (Command)" counsel-linux-app-format-function-name-first)
          (const :tag "Name - Comment" counsel-linux-app-format-function-name-only)
          (const :tag "Command" counsel-linux-app-format-function-command-only)
          (function :tag "Custom")))

(defvar counsel-linux-apps-faulty nil
  "List of faulty desktop files.")

(defvar counsel--linux-apps-cache nil
  "Cache of desktop files data.")

(defvar counsel--linux-apps-cached-files nil
  "List of cached desktop files.")

(defvar counsel--linux-apps-cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar counsel--linux-apps-cache-format-function nil
  "The function used to format the cached Linux application menu.")

(defun counsel-linux-app-format-function-default (name comment exec)
  "Default Linux application name formatter.
NAME is the name of the application, COMMENT its comment and EXEC
the command to launch it."
  (format "% -45s: %s%s"
          (propertize exec 'face 'font-lock-builtin-face)
          name
          (if comment
              (concat " - " comment)
            "")))

(defun counsel-linux-app-format-function-name-first (name comment exec)
  "Format Linux application names with the NAME (and COMMENT) first.
EXEC is the command to launch the application."
  (format "%s%s (%s)"
          name
          (if comment
              (concat " - " comment)
            "")
          (propertize exec 'face 'font-lock-builtin-face)))

(defun counsel-linux-app-format-function-name-only (name comment _exec)
  "Format Linux application names with the NAME (and COMMENT) only."
  (format "%s%s"
          name
          (if comment
              (concat " - " comment)
            "")))

(defun counsel-linux-app-format-function-command-only (_name _comment exec)
  "Display only the command EXEC when formatting Linux application names."
  exec)

(defun counsel-linux-apps-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
        result)
    (dolist (dir counsel-linux-apps-directories)
      (when (file-exists-p dir)
        (let ((dir (file-name-as-directory dir)))
          (dolist (file (directory-files-recursively dir ".*\\.desktop$"))
            (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
              (unless (gethash id hash)
                (push (cons id file) result)
                (puthash id file hash)))))))
    result))

(defun counsel-linux-apps-parse (desktop-entries-alist)
  "Parse the given alist of Linux desktop entries.
Each entry in DESKTOP-ENTRIES-ALIST is a pair of ((id . file-name)).
Any desktop entries that fail to parse are recorded in
`counsel-linux-apps-faulty'."
  (let (result)
    (setq counsel-linux-apps-faulty nil)
    (dolist (entry desktop-entries-alist result)
      (let ((id (car entry))
            (file (cdr entry)))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
                (end (re-search-forward "^\\[" nil t))
                name comment exec)
            (catch 'break
              (unless start
                (push file counsel-linux-apps-faulty)
                (message "Warning: File %s has no [Desktop Entry] group" file)
                (throw 'break nil))

              (goto-char start)
              (when (re-search-forward "^\\(Hidden\\|NoDisplay\\) *= *\\(1\\|true\\) *$" end t)
                (throw 'break nil))
              (setq name (match-string 1))

              (goto-char start)
              (unless (re-search-forward "^Type *= *Application *$" end t)
                (throw 'break nil))
              (setq name (match-string 1))

              (goto-char start)
              (unless (re-search-forward "^Name *= *\\(.+\\)$" end t)
                (push file counsel-linux-apps-faulty)
                (message "Warning: File %s has no Name" file)
                (throw 'break nil))
              (setq name (match-string 1))

              (goto-char start)
              (when (re-search-forward "^Comment *= *\\(.+\\)$" end t)
                (setq comment (match-string 1)))

              (goto-char start)
              (unless (re-search-forward "^Exec *= *\\(.+\\)$" end t)
                ;; Don't warn because this can technically be a valid desktop file.
                (throw 'break nil))
              (setq exec (match-string 1))

              (goto-char start)
              (when (re-search-forward "^TryExec *= *\\(.+\\)$" end t)
                (let ((try-exec (match-string 1)))
                  (unless (locate-file try-exec exec-path nil #'file-executable-p)
                    (throw 'break nil))))

              (push
               (cons (funcall counsel-linux-app-format-function name comment exec) id)
               result))))))))

(defun counsel-linux-apps-list ()
  "Return list of all Linux desktop applications."
  (let* ((new-desktop-alist (counsel-linux-apps-list-desktop-files))
         (new-files (mapcar 'cdr new-desktop-alist)))
    (unless (and
             (eq counsel-linux-app-format-function
                 counsel--linux-apps-cache-format-function)
             (equal new-files counsel--linux-apps-cached-files)
             (null (cl-find-if
                    (lambda (file)
                      (time-less-p
                       counsel--linux-apps-cache-timestamp
                       (nth 5 (file-attributes file))))
                    new-files)))
      (setq counsel--linux-apps-cache (counsel-linux-apps-parse new-desktop-alist)
            counsel--linux-apps-cache-format-function counsel-linux-app-format-function
            counsel--linux-apps-cache-timestamp (current-time)
            counsel--linux-apps-cached-files new-files)))
  counsel--linux-apps-cache)


(defun counsel-linux-app-action-default (desktop-shortcut)
  "Launch DESKTOP-SHORTCUT."
  (setq desktop-shortcut (cdr desktop-shortcut))
  (call-process "gtk-launch" nil nil nil desktop-shortcut))

(defun counsel-linux-app-action-file (desktop-shortcut)
  "Launch DESKTOP-SHORTCUT with a selected file."
  (setq desktop-shortcut (cdr desktop-shortcut))
  (let ((file (read-file-name "Open: ")))
    (if file
        (call-process "gtk-launch" nil nil nil desktop-shortcut file)
      (user-error "Cancelled"))))

(defun counsel-linux-app-action-open-desktop (desktop-shortcut)
  "Open DESKTOP-SHORTCUT."
  (setq desktop-shortcut (cdr desktop-shortcut))
  (let ((file
         (cdr (assoc desktop-shortcut (counsel-linux-apps-list-desktop-files)))))
    (if file
        (find-file file)
      (user-error "Cancelled"))))

(ivy-set-actions
 'counsel-linux-app
 '(("f" counsel-linux-app-action-file "run on a file")
   ("d" counsel-linux-app-action-open-desktop "open desktop file")))

;;;###autoload
(defun counsel-linux-app ()
  "Launch a Linux desktop application, similar to Alt-<F2>."
  (interactive)
  (ivy-read "Run a command: " (counsel-linux-apps-list)
            :action #'counsel-linux-app-action-default
            :caller 'counsel-linux-app))

;;** `counsel-company'
(defvar company-candidates)
(defvar company-point)
(defvar company-common)
(declare-function company-complete "ext:company")
(declare-function company-mode "ext:company")
(declare-function company-complete-common "ext:company")

;;;###autoload
(defun counsel-company ()
  "Complete using `company-candidates'."
  (interactive)
  (company-mode 1)
  (unless company-candidates
    (company-complete))
  (when company-point
    (when (looking-back company-common (line-beginning-position))
      (setq ivy-completion-beg (match-beginning 0))
      (setq ivy-completion-end (match-end 0)))
    (ivy-read "company cand: " company-candidates
              :action #'ivy-completion-in-region-action)))

;;;** `counsel-colors'
(defun counsel-colors--best-contrast-color (color)
  "Choose the best-contrast foreground color for a background color COLOR.

Use the relative luminance formula to improve the perceived contrast.
If the relative luminance is beyond a given threshold, in this case a
midpoint, then the chosen color is black, otherwise is white.  This
helps to improve the contrast and readability of a text regardless of
the background color."
  (let ((rgb (color-name-to-rgb color)))
    (if rgb
        (if (>
             (+ (* (nth 0 rgb) 0.299)
                (* (nth 1 rgb) 0.587)
                (* (nth 2 rgb) 0.114))
             0.5)
            "#000000"
          "#FFFFFF")
      color)))

(defun counsel-colors--update-highlight (cand)
  "Update the highlight face for the current candidate CAND.

This is necessary because the default `ivy-current-match' face
background mask most of the colors and you can not see the current
candidate color when is selected, which is counter-intuitive and not
user friendly.  The default Emacs command `list-colors-display' have
the same problem."
  (when (> (length cand) 0)
    (let ((color (substring-no-properties cand 26 33)))
      (face-remap-add-relative
       'ivy-current-match
       :background color
       ;; Another alternatives like use the attribute
       ;; `distant-foreground' or the function `color-complement-hex'
       ;; do not work well here because they use the absolute
       ;; luminance difference between the colors, when the human eye
       ;; does not perceive all the colors with the same brightness.
       :foreground (counsel-colors--best-contrast-color color)))))

(defun counsel-colors-action-insert-name (x)
  "Insert the X color name."
  (let ((color (car (split-string (substring x 0 25)))))
    (insert color)))

(defun counsel-colors-action-insert-hex (x)
  "Insert the X color hexadecimal rgb value."
  (let ((rgb (substring x 26 33)))
    (insert rgb)))

(defun counsel-colors-action-kill-name (x)
  "Kill the X color name."
  (let ((color (car (split-string (substring x 0 25)))))
    (kill-new color)))

(defun counsel-colors-action-kill-hex (x)
  "Kill the X color hexadecimal rgb value."
  (let ((rgb (substring x 26 33)))
    (kill-new rgb)))

;;** `counsel-colors-emacs'
(ivy-set-actions
 'counsel-colors-emacs
 '(("n" counsel-colors-action-insert-name "insert color name")
   ("h" counsel-colors-action-insert-hex "insert color hexadecimal value")
   ("N" counsel-colors-action-kill-name "kill color name")
   ("H" counsel-colors-action-kill-hex "kill color hexadecimal value")))

(defvar counsel-colors-emacs-history nil
  "History for `counsel-colors-emacs'.")

(defun counsel-colors--name-to-hex (color)
  "Return hexadecimal rgb value of a color from his name COLOR."
  (apply 'color-rgb-to-hex (color-name-to-rgb color)))

;;;###autoload
(defun counsel-colors-emacs ()
  "Show a list of all supported colors for a particular frame.

You can insert or kill the name or the hexadecimal rgb value of the
selected candidate."
  (interactive)
  (let ((minibuffer-allow-text-properties t))
    (ivy-read "%d Emacs color: "
              (mapcar (lambda (x)
                        (concat
                         (propertize
                          (format "%-25s" (car x))
                          'result (car x))
                         (propertize
                          (format "%8s  "
                                  (counsel-colors--name-to-hex (car x)))
                          'face (list :foreground (car x)))
                         (propertize
                          (format "%10s" " ")
                          'face (list :background (car x)))
                         (propertize
                          (format "  %-s" (mapconcat #'identity (cdr x) ", "))
                          'face (list :foreground (car x)))))
                      (list-colors-duplicates))
              :require-match t
              :update-fn (lambda ()
                           (counsel-colors--update-highlight (ivy-state-current ivy-last)))
              :action #'counsel-colors-action-insert-name
              :history 'counsel-colors-emacs-history
              :caller 'counsel-colors-emacs
              :sort nil)))

;;** `counsel-colors-web'
(defvar counsel-colors--web-colors-alist
  '(("aliceblue"            .  "#f0f8ff")
    ("antiquewhite"         .  "#faebd7")
    ("aqua"                 .  "#00ffff")
    ("aquamarine"           .  "#7fffd4")
    ("azure"                .  "#f0ffff")
    ("beige"                .  "#f5f5dc")
    ("bisque"               .  "#ffe4c4")
    ("black"                .  "#000000")
    ("blanchedalmond"       .  "#ffebcd")
    ("blue"                 .  "#0000ff")
    ("blueviolet"           .  "#8a2be2")
    ("brown"                .  "#a52a2a")
    ("burlywood"            .  "#deb887")
    ("cadetblue"            .  "#5f9ea0")
    ("chartreuse"           .  "#7fff00")
    ("chocolate"            .  "#d2691e")
    ("coral"                .  "#ff7f50")
    ("cornflowerblue"       .  "#6495ed")
    ("cornsilk"             .  "#fff8dc")
    ("crimson"              .  "#dc143c")
    ("cyan"                 .  "#00ffff")
    ("darkblue"             .  "#00008b")
    ("darkcyan"             .  "#008b8b")
    ("darkgoldenrod"        .  "#b8860b")
    ("darkgray"             .  "#a9a9a9")
    ("darkgreen"            .  "#006400")
    ("darkkhaki"            .  "#bdb76b")
    ("darkmagenta"          .  "#8b008b")
    ("darkolivegreen"       .  "#556b2f")
    ("darkorange"           .  "#ff8c00")
    ("darkorchid"           .  "#9932cc")
    ("darkred"              .  "#8b0000")
    ("darksalmon"           .  "#e9967a")
    ("darkseagreen"         .  "#8fbc8f")
    ("darkslateblue"        .  "#483d8b")
    ("darkslategray"        .  "#2f4f4f")
    ("darkturquoise"        .  "#00ced1")
    ("darkviolet"           .  "#9400d3")
    ("deeppink"             .  "#ff1493")
    ("deepskyblue"          .  "#00bfff")
    ("dimgray"              .  "#696969")
    ("dodgerblue"           .  "#1e90ff")
    ("firebrick"            .  "#b22222")
    ("floralwhite"          .  "#fffaf0")
    ("forestgreen"          .  "#228b22")
    ("fuchsia"              .  "#ff00ff")
    ("gainsboro"            .  "#dcdcdc")
    ("ghostwhite"           .  "#f8f8ff")
    ("goldenrod"            .  "#daa520")
    ("gold"                 .  "#ffd700")
    ("gray"                 .  "#808080")
    ("green"                .  "#008000")
    ("greenyellow"          .  "#adff2f")
    ("honeydew"             .  "#f0fff0")
    ("hotpink"              .  "#ff69b4")
    ("indianred"            .  "#cd5c5c")
    ("indigo"               .  "#4b0082")
    ("ivory"                .  "#fffff0")
    ("khaki"                .  "#f0e68c")
    ("lavenderblush"        .  "#fff0f5")
    ("lavender"             .  "#e6e6fa")
    ("lawngreen"            .  "#7cfc00")
    ("lemonchiffon"         .  "#fffacd")
    ("lightblue"            .  "#add8e6")
    ("lightcoral"           .  "#f08080")
    ("lightcyan"            .  "#e0ffff")
    ("lightgoldenrodyellow" .  "#fafad2")
    ("lightgreen"           .  "#90ee90")
    ("lightgrey"            .  "#d3d3d3")
    ("lightpink"            .  "#ffb6c1")
    ("lightsalmon"          .  "#ffa07a")
    ("lightseagreen"        .  "#20b2aa")
    ("lightskyblue"         .  "#87cefa")
    ("lightslategray"       .  "#778899")
    ("lightsteelblue"       .  "#b0c4de")
    ("lightyellow"          .  "#ffffe0")
    ("lime"                 .  "#00ff00")
    ("limegreen"            .  "#32cd32")
    ("linen"                .  "#faf0e6")
    ("magenta"              .  "#ff00ff")
    ("maroon"               .  "#800000")
    ("mediumaquamarine"     .  "#66cdaa")
    ("mediumblue"           .  "#0000cd")
    ("mediumorchid"         .  "#ba55d3")
    ("mediumpurple"         .  "#9370d8")
    ("mediumseagreen"       .  "#3cb371")
    ("mediumslateblue"      .  "#7b68ee")
    ("mediumspringgreen"    .  "#00fa9a")
    ("mediumturquoise"      .  "#48d1cc")
    ("mediumvioletred"      .  "#c71585")
    ("midnightblue"         .  "#191970")
    ("mintcream"            .  "#f5fffa")
    ("mistyrose"            .  "#ffe4e1")
    ("moccasin"             .  "#ffe4b5")
    ("navajowhite"          .  "#ffdead")
    ("navy"                 .  "#000080")
    ("oldlace"              .  "#fdf5e6")
    ("olive"                .  "#808000")
    ("olivedrab"            .  "#6b8e23")
    ("orange"               .  "#ffa500")
    ("orangered"            .  "#ff4500")
    ("orchid"               .  "#da70d6")
    ("palegoldenrod"        .  "#eee8aa")
    ("palegreen"            .  "#98fb98")
    ("paleturquoise"        .  "#afeeee")
    ("palevioletred"        .  "#d87093")
    ("papayawhip"           .  "#ffefd5")
    ("peachpuff"            .  "#ffdab9")
    ("peru"                 .  "#cd853f")
    ("pink"                 .  "#ffc0cb")
    ("plum"                 .  "#dda0dd")
    ("powderblue"           .  "#b0e0e6")
    ("purple"               .  "#800080")
    ("rebeccapurple"        .  "#663399")
    ("red"                  .  "#ff0000")
    ("rosybrown"            .  "#bc8f8f")
    ("royalblue"            .  "#4169e1")
    ("saddlebrown"          .  "#8b4513")
    ("salmon"               .  "#fa8072")
    ("sandybrown"           .  "#f4a460")
    ("seagreen"             .  "#2e8b57")
    ("seashell"             .  "#fff5ee")
    ("sienna"               .  "#a0522d")
    ("silver"               .  "#c0c0c0")
    ("skyblue"              .  "#87ceeb")
    ("slateblue"            .  "#6a5acd")
    ("slategray"            .  "#708090")
    ("snow"                 .  "#fffafa")
    ("springgreen"          .  "#00ff7f")
    ("steelblue"            .  "#4682b4")
    ("tan"                  .  "#d2b48c")
    ("teal"                 .  "#008080")
    ("thistle"              .  "#d8bfd8")
    ("tomato"               .  "#ff6347")
    ("turquoise"            .  "#40e0d0")
    ("violet"               .  "#ee82ee")
    ("wheat"                .  "#f5deb3")
    ("white"                .  "#ffffff")
    ("whitesmoke"           .  "#f5f5f5")
    ("yellow"               .  "#ffff00")
    ("yellowgreen"          .  "#9acd32"))
  "These are the colors defined by the W3C consortium to use in CSS sheets.

All of these colors are compatible with any common browser.  The
colors gray, green, maroon and purple have alternative values as
defined by the X11 standard, here they follow the W3C one.")

(ivy-set-actions
 'counsel-colors-web
 '(("n" counsel-colors-action-insert-name "insert name")
   ("h" counsel-colors-action-insert-hex "insert hex")
   ("N" counsel-colors-action-kill-name "kill rgb")
   ("H" counsel-colors-action-kill-hex "kill hex")))

(defvar counsel-colors-web-history nil
  "History for `counsel-colors-web'.")

;;;###autoload
(defun counsel-colors-web ()
  "Show a list of all W3C web colors for use in CSS.

You can insert or kill the name or the hexadecimal rgb value of the
selected candidate."
  (interactive)
  (let ((minibuffer-allow-text-properties t))
    (ivy-read "%d Web color: "
              (mapcar (lambda (x)
                        (concat
                         (propertize
                          (format "%-25s" (car x)))
                         (propertize
                          (format "%8s  " (cdr x))
                          'face (list :foreground (car x)))
                         (propertize
                          (format "%10s" " ")
                          'face (list :background (cdr x)))))
                      counsel-colors--web-colors-alist)
              :require-match t
              :action #'counsel-colors-action-insert-name
              :update-fn (lambda ()
                           (counsel-colors--update-highlight (ivy-state-current ivy-last)))
              :history 'counsel-colors-web-history
              :caller 'counsel-colors-web
              :sort t)))


;;** `counsel-faces'
(defun counsel-faces-action-describe (x)
  "Describe the face X."
  (describe-face (intern x)))

(defun counsel-faces-action-customize (x)
  "Customize the face X."
  (customize-face (intern x)))

(ivy-set-actions
 'counsel-faces
 '(("d" counsel-faces-action-describe "describe face")
   ("c" counsel-faces-action-customize "customize face")
   ("i" insert "insert face name")
   ("k" kill-new "kill face name")))

(defvar counsel-faces-history nil
  "History for `counsel-faces'.")

(defvar counsel-faces--sample-text
  "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789"
  "Text string to display as the sample text for `counsel-faces'.")

(defvar counsel--faces-fmt nil)

(defun counsel--faces-format-function (cands)
  "Transform CANDS into a string for `counsel-faces'."
  (ivy--format-function-generic
   (lambda (str)
     (concat
      (format counsel--faces-fmt
              (ivy--add-face str 'ivy-current-match))
      (propertize counsel-faces--sample-text 'face (intern str))))
   (lambda (str)
     (concat
      (format counsel--faces-fmt
              str)
      (propertize counsel-faces--sample-text 'face (intern str))))
   cands
   "\n"))

(defun counsel-faces ()
  "Show a list of all defined faces.

You can describe, customize, insert or kill the name or selected
candidate."
  (interactive)
  (let* ((minibuffer-allow-text-properties t)
         (max-length
          (apply #'max
                 (mapcar
                  (lambda (x)
                    (length (symbol-name x)))
                  (face-list))))
         (counsel--faces-fmt (format "%%-%ds  " max-length))
         (ivy-format-function #'counsel--faces-format-function))
    (ivy-read "%d Face: " (face-list)
              :require-match t
              :action #'counsel-faces-action-describe
              :history 'counsel-faces-history
              :caller 'counsel-faces
              :sort t)))

;;** `counsel-command-history'
(defun counsel-command-history-action-eval (cmd)
  "Eval the command CMD."
    (eval (read cmd)))

(defun counsel-command-history-action-edit-and-eval (cmd)
  "Edit and eval the command CMD."
    (edit-and-eval-command "Eval: " (read cmd)))

(ivy-set-actions
 'counsel-command-history
 '(("r" counsel-command-history-action-eval           "eval command")
   ("e" counsel-command-history-action-edit-and-eval  "edit and eval command")))

(defun counsel-command-history ()
  "Show the history of commands."
  (interactive)
  (ivy-read "%d Command: " (mapcar #'prin1-to-string command-history)
          :require-match t
          :action #'counsel-command-history-action-eval
          :caller 'counsel-command-history))

;;** `counsel-org-agenda-headlines'
(defvar org-odd-levels-only)
(declare-function org-set-startup-visibility "org")
(declare-function org-show-entry "org")
(declare-function org-map-entries "org")
(declare-function org-heading-components "org")

(defun counsel-org-agenda-headlines-action-goto (headline)
  "Go to the `org-mode' agenda HEADLINE."
  (find-file (nth 1 headline))
  (org-set-startup-visibility)
  (goto-char (nth 2 headline))
  (org-show-entry))

(ivy-set-actions
 'counsel-org-agenda-headlines
 '(("g" counsel-org-agenda-headlines-action-goto "goto headline")))

(defvar counsel-org-agenda-headlines-history nil
  "History for `counsel-org-agenda-headlines'.")

(defun counsel-org-agenda-headlines--candidates ()
  "Return a list of completion candidates for `counsel-org-agenda-headlines'."
  (org-map-entries
   (lambda ()
     (let* ((components (org-heading-components))
            (level (make-string
                    (if org-odd-levels-only
                        (nth 1 components)
                      (nth 0 components))
                    ?*))
            (todo (nth 2 components))
            (priority (nth 3 components))
            (text (nth 4 components))
            (tags (nth 5 components)))
       (list
        (mapconcat
         'identity
         (cl-remove-if 'null
                       (list
                        level
                        todo
                        (if priority (format "[#%c]" priority))
                        text
                        tags))
         " ")
        (buffer-file-name) (point))))
   nil
   'agenda))

;;;###autoload
(defun counsel-org-agenda-headlines ()
  "Choose from headers of `org-mode' files in the agenda."
  (interactive)
  (let ((minibuffer-allow-text-properties t))
    (ivy-read "Org headline: "
              (counsel-org-agenda-headlines--candidates)
              :action #'counsel-org-agenda-headlines-action-goto
              :history 'counsel-org-agenda-headlines-history
              :caller 'counsel-org-agenda-headlines)))

;;** `counsel-irony'
;;;###autoload
(defun counsel-irony ()
  "Inline C/C++ completion using Irony."
  (interactive)
  (irony-completion-candidates-async 'counsel-irony-callback))

(defun counsel-irony-callback (candidates)
  "Callback function for Irony to search among CANDIDATES."
  (interactive)
  (let* ((symbol-bounds (irony-completion-symbol-bounds))
         (beg (car symbol-bounds))
         (end (cdr symbol-bounds))
         (prefix (buffer-substring-no-properties beg end)))
  (setq ivy-completion-beg beg
        ivy-completion-end end)
    (ivy-read "code: " (mapcar #'counsel-irony-annotate candidates)
              :predicate (lambda (candidate)
                           (string-prefix-p prefix (car candidate)))
              :caller 'counsel-irony
              :action 'ivy-completion-in-region-action)))

(defun counsel-irony-annotate (x)
  "Make Ivy candidate from Irony candidate X."
  (cons (concat (car x) (irony-completion-annotation x))
        (car x)))

(add-to-list 'ivy-display-functions-alist '(counsel-irony . ivy-display-function-overlay))

(declare-function irony-completion-candidates-async "ext:irony-completion")
(declare-function irony-completion-symbol-bounds "ext:irony-completion")
(declare-function irony-completion-annotation "ext:irony-completion")

;;** `counsel-apropos'
;;;###autoload
(defun counsel-apropos ()
  "Show all matching symbols.
See `apropos' for further information about what is considered
a symbol and how to search for them."
  (interactive)
  (ivy-read "Search for symbol (word list or regexp): "
            (counsel-symbol-list)
            :history 'counsel-apropos-history
            :action (lambda (pattern)
                      (when (= (length pattern) 0)
                        (user-error "Please specify a pattern"))
                      ;; If the user selected a candidate form the list, we use
                      ;; a pattern which matches only the selected symbol.
                      (if (memq this-command '(ivy-immediate-done ivy-alt-done))
                          ;; Regexp pattern are passed verbatim, other input is
                          ;; split into words.
                          (if (string-equal (regexp-quote pattern) pattern)
                              (apropos (split-string pattern "[ \t]+" t))
                            (apropos pattern))
                        (apropos (concat "^" pattern "$"))))
            :caller 'counsel-apropos))

(defun counsel-symbol-list ()
  "Return a list of all symbols."
  (let (cands)
    (mapatoms
     (lambda (symbol)
       (when (or (boundp symbol) (fboundp symbol))
         (push (symbol-name symbol) cands))))
    (delete "" cands)))

;;** `counsel-mode'
(defvar counsel-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (binding
              '((execute-extended-command . counsel-M-x)
                (describe-bindings . counsel-descbinds)
                (describe-function . counsel-describe-function)
                (describe-variable . counsel-describe-variable)
                (describe-face . counsel-describe-face)
                (find-file . counsel-find-file)
                (find-library . counsel-find-library)
                (imenu . counsel-imenu)
                (load-library . counsel-load-library)
                (load-theme . counsel-load-theme)
                (yank-pop . counsel-yank-pop)
                (info-lookup-symbol . counsel-info-lookup-symbol)
                (pop-mark . counsel-mark-ring)))
      (define-key map (vector 'remap (car binding)) (cdr binding)))
    map)
  "Map for `counsel-mode'.
Remaps built-in functions to counsel replacements.")

(defcustom counsel-mode-override-describe-bindings nil
  "Whether to override `describe-bindings' when `counsel-mode' is active."
  :group 'ivy
  :type 'boolean)

(defun counsel-list-buffers-with-mode (mode)
  "List all buffers with `major-mode' MODE.

MODE is a symbol."
  (save-current-buffer
    (let (bufs)
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (and (equal major-mode mode)
             (push (buffer-name buf) bufs)))
      (nreverse bufs))))

;;;###autoload
(defun counsel-switch-to-shell-buffer ()
  "Switch to a shell buffer, or create one."
  (interactive)
  (ivy-read "Switch to shell buffer: "
            (counsel-list-buffers-with-mode 'shell-mode)
            :action #'counsel-switch-to-buffer-or-window
            :caller 'counsel-switch-to-shell-buffer))

(defun counsel-switch-to-buffer-or-window (buffer-name)
  "Display buffer BUFFER-NAME and select its window.

This behaves as `switch-to-buffer', except when the buffer is
already visible; in that case, select the window corresponding to
the buffer."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (shell buffer-name)
      (let (window-of-buffer-visible)
        (catch 'found
          (walk-windows (lambda (window)
                          (and (equal (window-buffer window) buffer)
                               (throw 'found (setq window-of-buffer-visible window))))))
        (if window-of-buffer-visible
            (select-window window-of-buffer-visible)
          (switch-to-buffer buffer))))))

;;;###autoload
(define-minor-mode counsel-mode
  "Toggle Counsel mode on or off.
Turn Counsel mode on if ARG is positive, off otherwise. Counsel
mode remaps built-in emacs functions that have counsel
replacements. "
  :group 'ivy
  :global t
  :keymap counsel-mode-map
  :lighter " counsel"
  (if counsel-mode
      (progn
        (when (and (fboundp 'advice-add)
                   counsel-mode-override-describe-bindings)
          (advice-add #'describe-bindings :override #'counsel-descbinds))
        (define-key minibuffer-local-shell-command-map (kbd "C-r")
          'counsel-shell-command-history)
        (define-key read-expression-map (kbd "C-r")
          'counsel-expression-history))
    (when (fboundp 'advice-remove)
      (advice-remove #'describe-bindings #'counsel-descbinds))))

(provide 'counsel)

;;; counsel.el ends here
