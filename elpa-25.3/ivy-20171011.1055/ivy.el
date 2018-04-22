;;; ivy.el --- Incremental Vertical completYon -*- lexical-binding: t -*-

;; Copyright (C) 2015-2017  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/swiper
;; Version: 0.9.1
;; Package-Requires: ((emacs "24.1"))
;; Keywords: matching

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
;; This package provides `ivy-read' as an alternative to
;; `completing-read' and similar functions.
;;
;; There's no intricate code to determine the best candidate.
;; Instead, the user can navigate to it with `ivy-next-line' and
;; `ivy-previous-line'.
;;
;; The matching is done by splitting the input text by spaces and
;; re-building it into a regex.
;; So "for example" is transformed into "\\(for\\).*\\(example\\)".

;;; Code:
(require 'cl-lib)
(require 'ffap)
(require 'ivy-overlay)

;;* Customization
(defgroup ivy nil
  "Incremental vertical completion."
  :group 'convenience)

(defgroup ivy-faces nil
  "Font-lock faces for `ivy'."
  :group 'ivy
  :group 'faces)

(defface ivy-current-match
  '((((class color) (background light))
     :background "#1a4b77" :foreground "white")
    (((class color) (background dark))
     :background "#65a7e2" :foreground "black"))
  "Face used by Ivy for highlighting the current match.")

(defface ivy-minibuffer-match-face-1
  '((((class color) (background light))
     :background "#d3d3d3")
    (((class color) (background dark))
     :background "#555555"))
  "The background face for `ivy' minibuffer matches.")

(defface ivy-minibuffer-match-face-2
  '((((class color) (background light))
     :background "#e99ce8" :weight bold)
    (((class color) (background dark))
     :background "#777777" :weight bold))
  "Face for `ivy' minibuffer matches numbered 1 modulo 3.")

(defface ivy-minibuffer-match-face-3
  '((((class color) (background light))
     :background "#bbbbff" :weight bold)
    (((class color) (background dark))
     :background "#7777ff" :weight bold))
  "Face for `ivy' minibuffer matches numbered 2 modulo 3.")

(defface ivy-minibuffer-match-face-4
  '((((class color) (background light))
     :background "#ffbbff" :weight bold)
    (((class color) (background dark))
     :background "#8a498a" :weight bold))
  "Face for `ivy' minibuffer matches numbered 3 modulo 3.")

(defface ivy-confirm-face
  '((t :foreground "ForestGreen" :inherit minibuffer-prompt))
  "Face used by Ivy for a confirmation prompt.")

(defface ivy-match-required-face
  '((t :foreground "red" :inherit minibuffer-prompt))
  "Face used by Ivy for a match required prompt.")

(defface ivy-subdir
  '((t :inherit dired-directory))
  "Face used by Ivy for highlighting subdirs in the alternatives.")

(defface ivy-modified-buffer
  '((t :inherit default))
  "Face used by Ivy for highlighting modified file visiting buffers.")

(defface ivy-remote
  '((((class color) (background light))
     :foreground "#110099")
    (((class color) (background dark))
     :foreground "#7B6BFF"))
  "Face used by Ivy for highlighting remotes in the alternatives.")

(defface ivy-virtual
  '((t :inherit font-lock-builtin-face))
  "Face used by Ivy for matching virtual buffer names.")

(defface ivy-action
  '((t :inherit font-lock-builtin-face))
  "Face used by Ivy for displaying keys in `ivy-read-action'.")

(defface ivy-highlight-face
  '((t :inherit highlight))
  "Face used by Ivy to highlight certain candidates.")

(defface ivy-prompt-match
  '((t :inherit ivy-current-match))
  "Face used by Ivy for highlighting the selected prompt line.")

(setcdr (assoc load-file-name custom-current-group-alist) 'ivy)

(defcustom ivy-height 10
  "Number of lines for the minibuffer window."
  :type 'integer)

(defcustom ivy-count-format "%-4d "
  "The style to use for displaying the current candidate count for `ivy-read'.
Set this to \"\" to suppress the count visibility.
Set this to \"(%d/%d) \" to display both the index and the count."
  :type '(choice
          (const :tag "Count disabled" "")
          (const :tag "Count matches" "%-4d ")
          (const :tag "Count matches and show current match" "(%d/%d) ")
          string))

(defcustom ivy-add-newline-after-prompt nil
  "When non-nil, add a newline after the `ivy-read' prompt."
  :type 'boolean)

(defcustom ivy-wrap nil
  "When non-nil, wrap around after the first and the last candidate."
  :type 'boolean)

(defcustom ivy-display-style (unless (version< emacs-version "24.5") 'fancy)
  "The style for formatting the minibuffer.

By default, the matched strings are copied as is.

The fancy display style highlights matching parts of the regexp,
a behavior similar to `swiper'.

This setting depends on `add-face-text-property' - a C function
available as of Emacs 24.5.  Fancy style will render poorly in
earlier versions of Emacs."
  :type '(choice
          (const :tag "Plain" nil)
          (const :tag "Fancy" fancy)))

(defcustom ivy-on-del-error-function 'minibuffer-keyboard-quit
  "The handler for when `ivy-backward-delete-char' throws.
Usually a quick exit out of the minibuffer."
  :type 'function)

(defcustom ivy-extra-directories '("../" "./")
  "Add this to the front of the list when completing file names.
Only \"./\" and \"../\" apply here.  They appear in reverse order."
  :type '(repeat :tag "Dirs"
          (choice
           (const :tag "Parent Directory" "../")
           (const :tag "Current Directory" "./"))))

(defcustom ivy-use-virtual-buffers nil
  "When non-nil, add recent files and bookmarks to `ivy-switch-buffer'."
  :type 'boolean)

(defcustom ivy-display-function nil
  "Decide where to display the candidates.
This function takes a string with the current matching candidates
and has to display it somewhere.
See https://github.com/abo-abo/swiper/wiki/ivy-display-function."
  :type '(choice
          (const :tag "Minibuffer" nil)
          (const :tag "LV" ivy-display-function-lv)
          (const :tag "Popup" ivy-display-function-popup)
          (const :tag "Overlay" ivy-display-function-overlay)))

(defvar ivy-display-functions-alist
  '((ivy-completion-in-region . ivy-display-function-overlay))
  "An alist for customizing `ivy-display-function'.")

(defcustom ivy-completing-read-handlers-alist
  '((tmm-menubar . completing-read-default)
    (tmm-shortcut . completing-read-default)
    (bbdb-create . ivy-completing-read-with-empty-string-def)
    (auto-insert . ivy-completing-read-with-empty-string-def)
    (Info-on-current-buffer . ivy-completing-read-with-empty-string-def)
    (Info-follow-reference . ivy-completing-read-with-empty-string-def)
    (Info-menu . ivy-completing-read-with-empty-string-def)
    (Info-index . ivy-completing-read-with-empty-string-def)
    (Info-virtual-index . ivy-completing-read-with-empty-string-def)
    (info-display-manual . ivy-completing-read-with-empty-string-def)
    (webjump . ivy-completing-read-with-empty-string-def))
  "An alist of handlers to replace `completing-read' in `ivy-mode'."
  :type '(alist :key-type function :value-type function))

(defvar ivy-completing-read-ignore-handlers-depth -1
  "Used to avoid infinite recursion.

If `(minibuffer-depth)' equals this, `ivy-completing-read' will
act as if `ivy-completing-read-handlers-alist' is empty.")

(defvar ivy--actions-list nil
  "A list of extra actions per command.")

(defun ivy-set-actions (cmd actions)
  "Set CMD extra exit points to ACTIONS."
  (setq ivy--actions-list
        (plist-put ivy--actions-list cmd actions)))

(defun ivy-add-actions (cmd actions)
  "Add CMD extra exit points to ACTIONS."
  (setq ivy--actions-list
        (plist-put ivy--actions-list cmd
                   (delete-dups
                    (append
                     actions
                     (plist-get ivy--actions-list cmd))))))

(defvar ivy--prompts-list nil)

(defun ivy-set-prompt (caller prompt-fn)
  "Associate CALLER with PROMPT-FN.
PROMPT-FN is a function of no arguments that returns a prompt string."
  (setq ivy--prompts-list
        (plist-put ivy--prompts-list caller prompt-fn)))

(defvar ivy--display-transformers-list nil
  "A list of str->str transformers per command.")

(defun ivy-set-display-transformer (cmd transformer)
  "Set CMD a displayed candidate TRANSFORMER.

It's a lambda that takes a string one of the candidates in the
collection and returns a string for display, the same candidate
plus some extra information.

This lambda is called only on the `ivy-height' candidates that
are about to be displayed, not on the whole collection."
  (setq ivy--display-transformers-list
        (plist-put ivy--display-transformers-list cmd transformer)))

(defvar ivy--sources-list nil
  "A list of extra sources per command.")

(defun ivy-set-sources (cmd sources)
  "Attach to CMD a list of extra SOURCES.

Each static source is a function that takes no argument and
returns a list of strings.

The (original-source) determines the position of the original
dynamic source.

Extra dynamic sources aren't supported yet.

Example:

    (defun small-recentf ()
      (cl-subseq recentf-list 0 20))

    (ivy-set-sources
     'counsel-locate
     '((small-recentf)
       (original-source)))"
  (setq ivy--sources-list
        (plist-put ivy--sources-list cmd sources)))

(defvar ivy-current-prefix-arg nil
  "Prefix arg to pass to actions.
This is a global variable that is set by ivy functions for use in
action functions.")

;;* Keymap
(require 'delsel)
(defvar ivy-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-m") 'ivy-done)
    (define-key map (kbd "C-M-m") 'ivy-call)
    (define-key map (kbd "C-j") 'ivy-alt-done)
    (define-key map (kbd "C-M-j") 'ivy-immediate-done)
    (define-key map (kbd "TAB") 'ivy-partial-or-done)
    (define-key map [remap next-line] 'ivy-next-line)
    (define-key map [remap previous-line] 'ivy-previous-line)
    (define-key map (kbd "C-s") 'ivy-next-line-or-history)
    (define-key map (kbd "C-r") 'ivy-reverse-i-search)
    (define-key map (kbd "SPC") 'self-insert-command)
    (define-key map [remap delete-backward-char] 'ivy-backward-delete-char)
    (define-key map [remap backward-delete-char-untabify] 'ivy-backward-delete-char)
    (define-key map [remap backward-kill-word] 'ivy-backward-kill-word)
    (define-key map [remap delete-char] 'ivy-delete-char)
    (define-key map [remap forward-char] 'ivy-forward-char)
    (define-key map [remap kill-word] 'ivy-kill-word)
    (define-key map [remap beginning-of-buffer] 'ivy-beginning-of-buffer)
    (define-key map [remap end-of-buffer] 'ivy-end-of-buffer)
    (define-key map (kbd "M-n") 'ivy-next-history-element)
    (define-key map (kbd "M-p") 'ivy-previous-history-element)
    (define-key map (kbd "C-g") 'minibuffer-keyboard-quit)
    (define-key map [remap scroll-up-command] 'ivy-scroll-up-command)
    (define-key map [remap scroll-down-command] 'ivy-scroll-down-command)
    (define-key map (kbd "<next>") 'ivy-scroll-up-command)
    (define-key map (kbd "<prior>") 'ivy-scroll-down-command)
    (define-key map (kbd "C-v") 'ivy-scroll-up-command)
    (define-key map (kbd "M-v") 'ivy-scroll-down-command)
    (define-key map (kbd "C-M-n") 'ivy-next-line-and-call)
    (define-key map (kbd "C-M-p") 'ivy-previous-line-and-call)
    (define-key map (kbd "M-r") 'ivy-toggle-regexp-quote)
    (define-key map (kbd "M-j") 'ivy-yank-word)
    (define-key map (kbd "M-i") 'ivy-insert-current)
    (define-key map (kbd "C-o") 'hydra-ivy/body)
    (define-key map (kbd "M-o") 'ivy-dispatching-done)
    (define-key map (kbd "C-M-o") 'ivy-dispatching-call)
    (define-key map [remap kill-line] 'ivy-kill-line)
    (define-key map (kbd "S-SPC") 'ivy-restrict-to-matches)
    (define-key map [remap kill-ring-save] 'ivy-kill-ring-save)
    (define-key map (kbd "C-'") 'ivy-avy)
    (define-key map (kbd "C-M-a") 'ivy-read-action)
    (define-key map (kbd "C-c C-o") 'ivy-occur)
    (define-key map (kbd "C-c C-a") 'ivy-toggle-ignore)
    (define-key map (kbd "C-c C-s") 'ivy-rotate-sort)
    (define-key map [remap describe-mode] 'ivy-help)
    map)
  "Keymap used in the minibuffer.")
(autoload 'hydra-ivy/body "ivy-hydra" "" t)

(defvar ivy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap switch-to-buffer]
      'ivy-switch-buffer)
    (define-key map [remap switch-to-buffer-other-window]
      'ivy-switch-buffer-other-window)
    map)
  "Keymap for `ivy-mode'.")

;;* Globals
(cl-defstruct ivy-state
  prompt collection
  predicate require-match initial-input
  history preselect keymap update-fn sort
  ;; The frame in which `ivy-read' was called
  frame
  ;; The window in which `ivy-read' was called
  window
  ;; The buffer in which `ivy-read' was called
  buffer
  ;; The value of `ivy-text' to be used by `ivy-occur'
  text
  action
  unwind
  re-builder
  matcher
  ;; When this is non-nil, call it for each input change to get new candidates
  dynamic-collection
  ;; A lambda that transforms candidates only for display
  display-transformer-fn
  directory
  caller
  current
  def)

(defvar ivy-last (make-ivy-state)
  "The last parameters passed to `ivy-read'.

This should eventually become a stack so that you could use
`ivy-read' recursively.")

(defvar ivy-recursive-last nil)

(defvar ivy-recursive-restore t
  "When non-nil, restore the above state when exiting the minibuffer.
This variable is let-bound to nil by functions that take care of
the restoring themselves.")

(defsubst ivy-set-action (action)
  "Set the current `ivy-last' field to ACTION."
  (setf (ivy-state-action ivy-last) action))

(defun ivy-thing-at-point ()
  "Return a string that corresponds to the current thing at point."
  (or
   (thing-at-point 'url)
   (and (eq (ivy-state-collection ivy-last) 'read-file-name-internal)
        (ffap-file-at-point))
   (let (s)
     (cond ((stringp (setq s (thing-at-point 'symbol)))
            (if (string-match "\\`[`']?\\(.*?\\)'?\\'" s)
                (match-string 1 s)
              s))
           ((looking-at "(+\\(\\(?:\\sw\\|\\s_\\)+\\)\\_>")
            (match-string-no-properties 1))
           (t
            "")))))

(defvar ivy-history nil
  "History list of candidates entered in the minibuffer.

Maximum length of the history list is determined by the value
of `history-length'.")

(defvar ivy--directory nil
  "Current directory when completing file names.")

(defvar ivy--length 0
  "Store the amount of viable candidates.")

(defvar ivy-text ""
  "Store the user's string as it is typed in.")

(defvar ivy--index 0
  "Store the index of the current candidate.")

(defvar ivy-exit nil
  "Store `done' if the completion was successfully selected.
Otherwise, store nil.")

(defvar ivy--all-candidates nil
  "Store the candidates passed to `ivy-read'.")

(defvar ivy--extra-candidates '((original-source))
  "Store candidates added by the extra sources.

This is an internal-use alist.  Each key is a function name, or
original-source (which represents where the current dynamic
candidates should go).

Each value is an evaluation of the function, in case of static
sources.  These values will subsequently be filtered on `ivy-text'.

This variable is set by `ivy-read' and used by `ivy--set-candidates'.")

(defcustom ivy-use-ignore-default t
  "The default policy for user-configured candidate filtering."
  :type '(choice
          (const :tag "Ignore ignored always" always)
          (const :tag "Ignore ignored when others exist" t)
          (const :tag "Don't ignore" nil)))

(defvar ivy-use-ignore t
  "Store policy for user-configured candidate filtering.
This may be changed dynamically by `ivy-toggle-ignore'.
Use `ivy-use-ignore-default' for a permanent configuration.")

(defvar ivy--default nil
  "Default initial input.")

(defvar ivy--prompt nil
  "Store the format-style prompt.
When non-nil, it should contain at least one %d.")

(defvar ivy--prompt-extra ""
  "Temporary modifications to the prompt.")

(defvar ivy--old-re nil
  "Store the old regexp.
Either a string or a list for `ivy-re-match'.")

(defvar ivy--old-cands nil
  "Store the candidates matched by `ivy--old-re'.")

(defvar ivy--regex-function 'ivy--regex
  "Current function for building a regex.")

(defvar ivy--highlight-function 'ivy--highlight-default
  "Current function for formatting the candidates.")

(defvar ivy--subexps 0
  "Number of groups in the current `ivy--regex'.")

(defvar ivy--full-length nil
  "The total amount of candidates when :dynamic-collection is non-nil.")

(defvar ivy--old-text ""
  "Store old `ivy-text' for dynamic completion.")

(defcustom ivy-case-fold-search-default 'auto
  "The default value for `ivy-case-fold-search'."
  :type '(choice
          (const :tag "Auto" auto)
          (const :tag "Always" always)
          (const :tag "Never" nil)))

(defvar ivy-case-fold-search ivy-case-fold-search-default
  "Store the current overriding `case-fold-search'.")

(defvar Info-current-file)

(defun ivy-re-to-str (re)
  (if (stringp re)
      re
    (caar re)))

(eval-and-compile
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL."
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var)))))
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defmacro ivy-quit-and-run (&rest body)
  "Quit the minibuffer and run BODY afterwards."
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (minibuffer-keyboard-quit)))

(defun ivy-exit-with-action (action)
  "Quit the minibuffer and call ACTION afterwards."
  (ivy-set-action
   `(lambda (x)
      (funcall ',action x)
      (ivy-set-action ',(ivy-state-action ivy-last))))
  (setq ivy-exit 'done)
  (exit-minibuffer))

(defmacro with-ivy-window (&rest body)
  "Execute BODY in the window from which `ivy-read' was called."
  (declare (indent 0)
           (debug t))
  `(with-selected-window (ivy--get-window ivy-last)
     ,@body))

(defun ivy--done (text)
  "Insert TEXT and exit minibuffer."
  (insert
   (setf (ivy-state-current ivy-last)
         (if (and ivy--directory
                  (not (eq (ivy-state-history ivy-last) 'grep-files-history)))
             (expand-file-name text ivy--directory)
           text)))
  (setq ivy-exit 'done)
  (exit-minibuffer))

(defcustom ivy-use-selectable-prompt nil
  "When non-nil, make the prompt line selectable like a candidate.

The prompt line can be selected by calling `ivy-previous-line' when the first
regular candidate is selected.  Both actions `ivy-done' and `ivy-alt-done',
when called on a selected prompt, are forwarded to `ivy-immediate-done', which
results to the same as calling `ivy-immediate-done' explicitely when a regular
candidate is selected.

Note that if `ivy-wrap' is set to t, calling `ivy-previous-line' when the
prompt is selected wraps around to the last candidate, while calling
`ivy-next-line' on the last candidate wraps around to the first
candidate, not the prompt."
  :type 'boolean)

(defun ivy--prompt-selectable-p ()
  "Return t if the prompt line is selectable."
  (and ivy-use-selectable-prompt
       (memq (ivy-state-require-match ivy-last)
             '(nil confirm confirm-after-completion))))

(defun ivy--prompt-selected-p ()
  "Return t if the prompt line is selected."
  (and (ivy--prompt-selectable-p)
       (= ivy--index -1)))

;;* Commands
(defun ivy-done ()
  "Exit the minibuffer with the selected candidate."
  (interactive)
  (if (ivy--prompt-selected-p)
      (ivy-immediate-done)
    (setq ivy-current-prefix-arg current-prefix-arg)
    (delete-minibuffer-contents)
    (cond ((or (> ivy--length 0)
               ;; the action from `ivy-dispatching-done' may not need a
               ;; candidate at all
               (eq this-command 'ivy-dispatching-done))
           (ivy--done (ivy-state-current ivy-last)))
          ((memq (ivy-state-collection ivy-last)
                 '(read-file-name-internal internal-complete-buffer))
           (if (or (not (eq confirm-nonexistent-file-or-buffer t))
                   (equal " (confirm)" ivy--prompt-extra))
               (ivy--done ivy-text)
             (setq ivy--prompt-extra " (confirm)")
             (insert ivy-text)
             (ivy--exhibit)))
          ((memq (ivy-state-require-match ivy-last)
                 '(nil confirm confirm-after-completion))
           (ivy--done ivy-text))
          (t
           (setq ivy--prompt-extra " (match required)")
           (insert ivy-text)
           (ivy--exhibit)))))

(defvar ivy-read-action-format-function 'ivy-read-action-format-default
  "Function used to transform the actions list into a docstring.")

(defun ivy-read-action-format-default (actions)
  "Create a docstring from ACTIONS.

ACTIONS is a list.  Each list item is a list of 3 items:
key (a string), cmd and doc (a string)."
  (format "%s\n%s\n"
          (if (eq this-command 'ivy-read-action)
              "Select action: "
            (ivy-state-current ivy-last))
          (mapconcat
           (lambda (x)
             (format "%s: %s"
                     (propertize
                      (car x)
                      'face 'ivy-action)
                     (nth 2 x)))
           actions
           "\n")))

(defun ivy-read-action ()
  "Change the action to one of the available ones.

Return nil for `minibuffer-keyboard-quit' or wrong key during the
selection, non-nil otherwise."
  (interactive)
  (let ((actions (ivy-state-action ivy-last)))
    (if (null (ivy--actionp actions))
        t
      (let* ((hint (funcall ivy-read-action-format-function (cdr actions)))
             (resize-mini-windows t)
             (key (string (read-key hint)))
             (action-idx (cl-position-if
                          (lambda (x) (equal (car x) key))
                          (cdr actions))))
        (cond ((member key '("" ""))
               nil)
              ((null action-idx)
               (message "%s is not bound" key)
               nil)
              (t
               (message "")
               (setcar actions (1+ action-idx))
               (ivy-set-action actions)))))))

(defun ivy-shrink-after-dispatching ()
  "Shrink the window after dispatching when action list is too large."
  (let ((window (selected-window)))
    (window-resize window (- ivy-height (window-height window)))))

(defun ivy-dispatching-done ()
  "Select one of the available actions and call `ivy-done'."
  (interactive)
  (when (ivy-read-action)
    (ivy-done))
  (ivy-shrink-after-dispatching))

(defun ivy-dispatching-call ()
  "Select one of the available actions and call `ivy-call'."
  (interactive)
  (setq ivy-current-prefix-arg current-prefix-arg)
  (let ((actions (copy-sequence (ivy-state-action ivy-last))))
    (unwind-protect
         (when (ivy-read-action)
           (ivy-call))
      (ivy-set-action actions)))
  (ivy-shrink-after-dispatching))

(defun ivy-build-tramp-name (x)
  "Reconstruct X into a path.
Is is a cons cell, related to `tramp-get-completion-function'."
  (let ((user (car x))
        (domain (cadr x)))
    (if user
        (concat user "@" domain)
      domain)))

(declare-function tramp-get-completion-function "tramp")
(declare-function Info-find-node "info")

(defun ivy-alt-done (&optional arg)
  "Exit the minibuffer with the selected candidate.
When ARG is t, exit with current text, ignoring the candidates."
  (interactive "P")
  (setq ivy-current-prefix-arg current-prefix-arg)
  (cond ((or arg
             (ivy--prompt-selected-p))
         (ivy-immediate-done))
        (ivy--directory
         (ivy--directory-done))
        ((eq (ivy-state-collection ivy-last) 'Info-read-node-name-1)
         (if (member (ivy-state-current ivy-last) '("(./)" "(../)"))
             (ivy-quit-and-run
              (ivy-read "Go to file: " 'read-file-name-internal
                        :action (lambda (x)
                                  (Info-find-node
                                   (expand-file-name x ivy--directory)
                                   "Top"))))
           (ivy-done)))
        (t
         (ivy-done))))

(defun ivy--directory-done ()
  "Handle exit from the minibuffer when completing file names."
  (let (dir)
    (cond
      ((equal ivy-text "/sudo::")
       (setq dir (concat ivy-text (expand-file-name ivy--directory)))
       (ivy--cd dir)
       (ivy--exhibit))
      ((and
        (> ivy--length 0)
        (not (string= (ivy-state-current ivy-last) "./"))
        (setq dir (ivy-expand-file-if-directory (ivy-state-current ivy-last))))
       (ivy--cd dir)
       (ivy--exhibit))
      ((and (not (string= ivy-text ""))
            (ignore-errors (file-exists-p ivy-text)))
       (if (file-directory-p ivy-text)
           (ivy--cd (expand-file-name
                     (file-name-as-directory ivy-text) ivy--directory))
         (ivy-done)))
      ((or (and (equal ivy--directory "/")
                (string-match "\\`[^/]+:.*:.*\\'" ivy-text))
           (string-match "\\`/[^/]+:.*:.*\\'" ivy-text))
       (ivy-done))
      ((or (and (equal ivy--directory "/")
                (cond ((string-match
                        "\\`\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
                        ivy-text))
                      ((string-match
                        "\\`\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
                        (ivy-state-current ivy-last))
                       (setq ivy-text (ivy-state-current ivy-last)))))
           (string-match
            "\\`/\\([^/]+?\\):\\(?:\\(.*\\)@\\)?\\(.*\\)\\'"
            ivy-text))
       (let ((method (match-string 1 ivy-text))
             (user (match-string 2 ivy-text))
             (rest (match-string 3 ivy-text))
             res)
         (require 'tramp)
         (dolist (x (tramp-get-completion-function method))
           (setq res (append res (funcall (car x) (cadr x)))))
         (setq res (delq nil res))
         (when user
           (dolist (x res)
             (setcar x user)))
         (setq res (cl-delete-duplicates res :test #'equal))
         (let* ((old-ivy-last ivy-last)
                (enable-recursive-minibuffers t)
                (host (ivy-read "user@host: "
                                (mapcar #'ivy-build-tramp-name res)
                                :initial-input rest)))
           (setq ivy-last old-ivy-last)
           (when host
             (setq ivy--directory "/")
             (ivy--cd (concat "/" method ":" host ":"))))))
      (t
       (ivy-done)))))

(defun ivy-expand-file-if-directory (file-name)
  "Expand FILE-NAME as directory.
When this directory doesn't exist, return nil."
  (when (stringp file-name)
    (let ((full-name
           ;; Ignore host name must not match method "ssh"
           (ignore-errors
             (file-name-as-directory
              (expand-file-name file-name ivy--directory)))))
      (when (and full-name (file-directory-p full-name))
        full-name))))

(defcustom ivy-tab-space nil
  "When non-nil, `ivy-partial-or-done' should insert a space."
  :type 'boolean)

(defun ivy-partial-or-done ()
  "Complete the minibuffer text as much as possible.
If the text hasn't changed as a result, forward to `ivy-alt-done'."
  (interactive)
  (if (and (eq (ivy-state-collection ivy-last) #'read-file-name-internal)
           (or (and (equal ivy--directory "/")
                    (string-match "\\`[^/]+:.*\\'" ivy-text))
               (string-match "\\`/" ivy-text)))
      (let ((default-directory ivy--directory)
            dir)
        (minibuffer-complete)
        (setq ivy-text (ivy--input))
        (when (setq dir (ivy-expand-file-if-directory ivy-text))
          (ivy--cd dir)))
    (or (ivy-partial)
        (when (or (eq this-command last-command)
                  (eq ivy--length 1))
          (ivy-alt-done)))))

(defun ivy-partial ()
  "Complete the minibuffer text as much as possible."
  (interactive)
  (let* ((parts (or (split-string ivy-text " " t) (list "")))
         (postfix (car (last parts)))
         (case-fold-search (and ivy-case-fold-search
                                (or (eq ivy-case-fold-search 'always)
                                    (string= ivy-text (downcase ivy-text)))))
         (completion-ignore-case case-fold-search)
         (startp (string-match "^\\^" postfix))
         (new (try-completion (if startp
                                  (substring postfix 1)
                                postfix)
                              (if (ivy-state-dynamic-collection ivy-last)
                                  ivy--all-candidates
                                (mapcar (lambda (str)
                                          (let ((i (string-match postfix str)))
                                            (when i
                                              (substring str i))))
                                        ivy--old-cands)))))
    (cond ((eq new t) nil)
          ((string= new ivy-text) nil)
          (new
           (delete-region (minibuffer-prompt-end) (point-max))
           (setcar (last parts)
                   (if startp
                       (concat "^" new)
                     new))
           (insert (mapconcat #'identity parts " ")
                   (if ivy-tab-space " " ""))
           t))))

(defvar ivy-completion-beg nil
  "Completion bounds start.")

(defvar ivy-completion-end nil
  "Completion bounds end.")

(defun ivy-immediate-done ()
  "Exit the minibuffer with current input instead of current candidate."
  (interactive)
  (delete-minibuffer-contents)
  (insert (setf (ivy-state-current ivy-last)
                (if (and ivy--directory
                         (not (eq (ivy-state-history ivy-last)
                                  'grep-files-history)))
                    (expand-file-name ivy-text ivy--directory)
                  ivy-text)))
  (setq ivy-completion-beg ivy-completion-end)
  (setq ivy-exit 'done)
  (exit-minibuffer))

;;;###autoload
(defun ivy-resume ()
  "Resume the last completion session."
  (interactive)
  (if (null (ivy-state-action ivy-last))
      (user-error "The last session isn't compatible with `ivy-resume'")
    (when (eq (ivy-state-caller ivy-last) 'swiper)
      (switch-to-buffer (ivy-state-buffer ivy-last)))
    (with-current-buffer (ivy-state-buffer ivy-last)
      (let ((default-directory (ivy-state-directory ivy-last)))
        (ivy-read
         (ivy-state-prompt ivy-last)
         (ivy-state-collection ivy-last)
         :predicate (ivy-state-predicate ivy-last)
         :require-match (ivy-state-require-match ivy-last)
         :initial-input ivy-text
         :history (ivy-state-history ivy-last)
         :preselect (unless (eq (ivy-state-collection ivy-last)
                                'read-file-name-internal)
                      (ivy-state-current ivy-last))
         :keymap (ivy-state-keymap ivy-last)
         :update-fn (ivy-state-update-fn ivy-last)
         :sort (ivy-state-sort ivy-last)
         :action (ivy-state-action ivy-last)
         :unwind (ivy-state-unwind ivy-last)
         :re-builder (ivy-state-re-builder ivy-last)
         :matcher (ivy-state-matcher ivy-last)
         :dynamic-collection (ivy-state-dynamic-collection ivy-last)
         :caller (ivy-state-caller ivy-last))))))

(defvar-local ivy-calling nil
  "When non-nil, call the current action when `ivy--index' changes.")

(defun ivy-set-index (index)
  "Set `ivy--index' to INDEX."
  (setq ivy--index index)
  (when ivy-calling
    (ivy--exhibit)
    (ivy-call)))

(defun ivy-beginning-of-buffer ()
  "Select the first completion candidate."
  (interactive)
  (ivy-set-index 0))

(defun ivy-end-of-buffer ()
  "Select the last completion candidate."
  (interactive)
  (ivy-set-index (1- ivy--length)))

(defun ivy-scroll-up-command ()
  "Scroll the candidates upward by the minibuffer height."
  (interactive)
  (ivy-set-index (min (1- (+ ivy--index ivy-height))
                      (1- ivy--length))))

(defun ivy-scroll-down-command ()
  "Scroll the candidates downward by the minibuffer height."
  (interactive)
  (ivy-set-index (max (1+ (- ivy--index ivy-height))
                      0)))

(defun ivy-minibuffer-grow ()
  "Grow the minibuffer window by 1 line."
  (interactive)
  (setq-local max-mini-window-height
              (cl-incf ivy-height)))

(defun ivy-minibuffer-shrink ()
  "Shrink the minibuffer window by 1 line."
  (interactive)
  (unless (<= ivy-height 2)
    (setq-local max-mini-window-height
                (cl-decf ivy-height))
    (window-resize (selected-window) -1)))

(defun ivy-next-line (&optional arg)
  "Move cursor vertically down ARG candidates."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((index (+ ivy--index arg)))
    (if (> index (1- ivy--length))
        (if ivy-wrap
            (ivy-beginning-of-buffer)
          (ivy-set-index (1- ivy--length)))
      (ivy-set-index index))))

(defun ivy-next-line-or-history (&optional arg)
  "Move cursor vertically down ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (if (string= ivy-text "")
      (ivy-previous-history-element 1)
    (ivy-next-line arg)))

(defun ivy-previous-line (&optional arg)
  "Move cursor vertically up ARG candidates."
  (interactive "p")
  (setq arg (or arg 1))
  (let ((index (- ivy--index arg))
        (min-index (or (and (ivy--prompt-selectable-p) -1)
                       0)))
    (if (< index min-index)
        (if ivy-wrap
            (ivy-end-of-buffer)
          (ivy-set-index min-index))
      (ivy-set-index index))))

(defun ivy-previous-line-or-history (arg)
  "Move cursor vertically up ARG candidates.
If the input is empty, select the previous history element instead."
  (interactive "p")
  (when (and (zerop ivy--index) (string= ivy-text ""))
    (ivy-previous-history-element 1))
  (ivy-previous-line arg))

(defun ivy-toggle-calling ()
  "Flip `ivy-calling'."
  (interactive)
  (when (setq ivy-calling (not ivy-calling))
    (ivy-call)))

(defun ivy-toggle-ignore ()
  "Toggle user-configured candidate filtering."
  (interactive)
  (setq ivy-use-ignore
        (if ivy-use-ignore
            nil
          (or ivy-use-ignore-default t)))
  ;; invalidate cache
  (setq ivy--old-cands nil))

(defun ivy--get-action (state)
  "Get the action function from STATE."
  (let ((action (ivy-state-action state)))
    (when action
      (if (functionp action)
          action
        (cadr (nth (car action) action))))))

(defun ivy--get-window (state)
  "Get the window from STATE."
  (if (ivy-state-p state)
      (let ((window (ivy-state-window state)))
        (if (window-live-p window)
            window
          (if (= (length (window-list)) 1)
              (selected-window)
            (next-window))))
    (selected-window)))

(defun ivy--actionp (x)
  "Return non-nil when X is a list of actions."
  (and x (listp x) (not (memq (car x) '(closure lambda)))))

(defcustom ivy-action-wrap nil
  "When non-nil, `ivy-next-action' and `ivy-prev-action' wrap."
  :type 'boolean)

(defun ivy-next-action ()
  "When the current action is a list, scroll it forwards."
  (interactive)
  (let ((action (ivy-state-action ivy-last)))
    (when (ivy--actionp action)
      (let ((len (1- (length action)))
            (idx (car action)))
        (if (>= idx len)
            (when ivy-action-wrap
              (setf (car action) 1))
          (cl-incf (car action)))))))

(defun ivy-prev-action ()
  "When the current action is a list, scroll it backwards."
  (interactive)
  (let ((action (ivy-state-action ivy-last)))
    (when (ivy--actionp action)
      (if (<= (car action) 1)
          (when ivy-action-wrap
            (setf (car action) (1- (length action))))
        (cl-decf (car action))))))

(defun ivy-action-name ()
  "Return the name associated with the current action."
  (let ((action (ivy-state-action ivy-last)))
    (if (ivy--actionp action)
        (format "[%d/%d] %s"
                (car action)
                (1- (length action))
                (nth 2 (nth (car action) action)))
      "[1/1] default")))

(defvar ivy-inhibit-action nil
  "When non-nil, `ivy-call' does nothing.

Example use:

    (let* ((ivy-inhibit-action t)
          (str (counsel-locate \"lispy.el\")))
     ;; do whatever with str - the corresponding file will not be opened
     )")

(defun ivy-recursive-restore ()
  "Restore the above state when exiting the minibuffer.
See variable `ivy-recursive-restore' for further information."
  (when (and ivy-recursive-last
             ivy-recursive-restore
             (not (eq ivy-last ivy-recursive-last)))
    (ivy--reset-state (setq ivy-last ivy-recursive-last))))

(defun ivy-call ()
  "Call the current action without exiting completion."
  (interactive)
  (unless
      (or
       ;; this is needed for testing in ivy-with which seems to call ivy-call
       ;; again, and this-command is nil in that case.
       (null this-command)
       (memq this-command '(ivy-done
                            ivy-alt-done
                            ivy-dispatching-done)))
    (setq ivy-current-prefix-arg current-prefix-arg))
  (unless ivy-inhibit-action
    (let ((action (ivy--get-action ivy-last)))
      (when action
        (let* ((collection (ivy-state-collection ivy-last))
               (x (cond
                    ;; Alist type.
                    ((and (consp collection)
                          (consp (car collection))
                          ;; Previously, the cdr of the selected
                          ;; candidate would be returned.  Now, the
                          ;; whole candidate is returned.
                          (let (idx)
                            (if (setq idx (get-text-property
                                           0 'idx (ivy-state-current ivy-last)))
                                (nth idx collection)
                              (assoc (ivy-state-current ivy-last)
                                     collection)))))
                    (ivy--directory
                     (expand-file-name
                      (ivy-state-current ivy-last)
                      ivy--directory))
                    ((equal (ivy-state-current ivy-last) "")
                     ivy-text)
                    (t
                     (ivy-state-current ivy-last)))))
          (if (eq action 'identity)
              (funcall action x)
            (select-window (ivy--get-window ivy-last))
            (prog1 (with-current-buffer (ivy-state-buffer ivy-last)
                     (unwind-protect (funcall action x)
                       (ivy-recursive-restore)))
              (unless (or (eq ivy-exit 'done)
                          (equal (selected-window)
                                 (active-minibuffer-window))
                          (null (active-minibuffer-window)))
                (select-window (active-minibuffer-window))))))))))

(defun ivy-call-and-recenter ()
  "Call action and recenter window according to the selected candidate."
  (interactive)
  (ivy-call)
  (with-ivy-window
    (recenter-top-bottom)))

(defun ivy-next-line-and-call (&optional arg)
  "Move cursor vertically down ARG candidates.
Call the permanent action if possible."
  (interactive "p")
  (ivy-next-line arg)
  (ivy--exhibit)
  (ivy-call))

(defun ivy-previous-line-and-call (&optional arg)
  "Move cursor vertically down ARG candidates.
Call the permanent action if possible."
  (interactive "p")
  (ivy-previous-line arg)
  (ivy--exhibit)
  (ivy-call))

(defun ivy-previous-history-element (arg)
  "Forward to `previous-history-element' with ARG."
  (interactive "p")
  (previous-history-element arg)
  (ivy--cd-maybe)
  (move-end-of-line 1)
  (ivy--maybe-scroll-history))

(defun ivy-next-history-element (arg)
  "Forward to `next-history-element' with ARG."
  (interactive "p")
  (if (and (= minibuffer-history-position 0)
           (equal ivy-text ""))
      (progn
        (insert ivy--default)
        (when (and (with-ivy-window (derived-mode-p 'prog-mode))
                   (eq (ivy-state-caller ivy-last) 'swiper)
                   (not (file-exists-p ivy--default))
                   (not (ffap-url-p ivy--default))
                   (not (ivy-state-dynamic-collection ivy-last))
                   (> (point) (minibuffer-prompt-end)))
          (undo-boundary)
          (insert "\\_>")
          (goto-char (minibuffer-prompt-end))
          (insert "\\_<")
          (forward-char (+ 2 (length ivy--default)))))
    (next-history-element arg))
  (ivy--cd-maybe)
  (move-end-of-line 1)
  (ivy--maybe-scroll-history))

(defvar ivy-ffap-url-functions nil
  "List of functions that check if the point is on a URL.")

(defun ivy--cd-maybe ()
  "Check if the current input points to a different directory.
If so, move to that directory, while keeping only the file name."
  (when ivy--directory
    (let ((input (ivy--input))
          url)
      (if (setq url (or (ffap-url-p input)
                        (with-ivy-window
                          (cl-reduce
                           (lambda (a b)
                             (or a (funcall b)))
                           ivy-ffap-url-functions
                           :initial-value nil))))
          (ivy-exit-with-action
           (lambda (_)
             (funcall ffap-url-fetcher url)))
        (setq input (expand-file-name input))
        (let ((file (file-name-nondirectory input))
              (dir (expand-file-name (file-name-directory input))))
          (if (string= dir ivy--directory)
              (progn
                (delete-minibuffer-contents)
                (insert file))
            (ivy--cd dir)
            (insert file)))))))

(defun ivy--maybe-scroll-history ()
  "If the selected history element has an index, scroll there."
  (let ((idx (ignore-errors
               (get-text-property
                (minibuffer-prompt-end)
                'ivy-index))))
    (when idx
      (ivy--exhibit)
      (ivy-set-index idx))))

(defun ivy--cd (dir)
  "When completing file names, move to directory DIR."
  (if (null ivy--directory)
      (error "Unexpected")
    (setq ivy--old-cands nil)
    (setq ivy--old-re nil)
    (ivy-set-index 0)
    (setq ivy--all-candidates
          (ivy--sorted-files (setq ivy--directory dir)))
    (setq ivy-text "")
    (delete-minibuffer-contents)))

(defun ivy-backward-delete-char ()
  "Forward to `backward-delete-char'.
On error (read-only), call `ivy-on-del-error-function'."
  (interactive)
  (if (and ivy--directory (= (minibuffer-prompt-end) (point)))
      (progn
        (ivy--cd (file-name-directory
                  (directory-file-name
                   (expand-file-name
                    ivy--directory))))
        (ivy--exhibit))
    (condition-case nil
        (backward-delete-char 1)
      (error
       (when ivy-on-del-error-function
         (funcall ivy-on-del-error-function))))))

(defun ivy-delete-char (arg)
  "Forward to `delete-char' ARG."
  (interactive "p")
  (unless (= (point) (line-end-position))
    (delete-char arg)))

(defun ivy-forward-char (arg)
  "Forward to `forward-char' ARG."
  (interactive "p")
  (unless (= (point) (line-end-position))
    (forward-char arg)))

(defun ivy-kill-word (arg)
  "Forward to `kill-word' ARG."
  (interactive "p")
  (unless (= (point) (line-end-position))
    (kill-word arg)))

(defun ivy-kill-line ()
  "Forward to `kill-line'."
  (interactive)
  (if (eolp)
      (kill-region (minibuffer-prompt-end) (point))
    (kill-line)))

(defun ivy-backward-kill-word ()
  "Forward to `backward-kill-word'."
  (interactive)
  (if (and ivy--directory (= (minibuffer-prompt-end) (point)))
      (progn
        (ivy--cd (file-name-directory
                  (directory-file-name
                   (expand-file-name
                    ivy--directory))))
        (ivy--exhibit))
    (ignore-errors
      (let ((pt (point)))
        (forward-word -1)
        (delete-region (point) pt)))))

(defvar ivy--regexp-quote 'regexp-quote
  "Store the regexp quoting state.")

(defun ivy-toggle-regexp-quote ()
  "Toggle the regexp quoting."
  (interactive)
  (setq ivy--old-re nil)
  (cl-rotatef ivy--regex-function ivy--regexp-quote))

(defvar avy-all-windows)
(defvar avy-action)
(defvar avy-keys)
(defvar avy-keys-alist)
(defvar avy-style)
(defvar avy-styles-alist)
(declare-function avy--process "ext:avy")
(declare-function avy--style-fn "ext:avy")

(defcustom ivy-format-function 'ivy-format-function-default
  "Function to transform the list of candidates into a string.
This string is inserted into the minibuffer."
  :type '(choice
          (const :tag "Default" ivy-format-function-default)
          (const :tag "Arrow prefix" ivy-format-function-arrow)
          (const :tag "Full line" ivy-format-function-line)))

(eval-after-load 'avy
  '(add-to-list 'avy-styles-alist '(ivy-avy . pre)))

(defun ivy-avy ()
  "Jump to one of the current ivy candidates."
  (interactive)
  (unless (require 'avy nil 'noerror)
    (error "Package avy isn't installed"))
  (let* ((avy-all-windows nil)
         (avy-keys (or (cdr (assq 'ivy-avy avy-keys-alist))
                       avy-keys))
         (avy-style (or (cdr (assq 'ivy-avy
                                   avy-styles-alist))
                        avy-style))
         (candidate
          (let ((candidates))
            (save-excursion
              (save-restriction
                (narrow-to-region
                 (window-start)
                 (window-end))
                (goto-char (point-min))
                (forward-line)
                (while (< (point) (point-max))
                  (push
                   (cons (point)
                         (selected-window))
                   candidates)
                  (forward-line))))
            (setq avy-action #'identity)
            (avy--process
             (nreverse candidates)
             (avy--style-fn avy-style)))))
    (when (number-or-marker-p candidate)
      (goto-char candidate)
      (when (eq ivy-format-function 'ivy-format-function-arrow)
        (forward-char 2))
      (ivy--done
       (buffer-substring-no-properties
        (point) (line-end-position))))))

(defun ivy-sort-file-function-default (x y)
  "Compare two files X and Y.
Prioritize directories."
  (if (get-text-property 0 'dirp x)
      (if (get-text-property 0 'dirp y)
          (string< x y)
        t)
    (if (get-text-property 0 'dirp y)
        nil
      (string< x y))))

(declare-function ido-file-extension-lessp "ido")

(defun ivy-sort-file-function-using-ido (x y)
  "Compare two files X and Y using `ido-file-extensions-order'.

This function is suitable as a replacement for
`ivy-sort-file-function-default' in `ivy-sort-functions-alist'."
  (if (and (bound-and-true-p ido-file-extensions-order))
      (ido-file-extension-lessp x y)
    (ivy-sort-file-function-default x y)))

(defcustom ivy-sort-functions-alist
  '((read-file-name-internal . ivy-sort-file-function-default)
    (internal-complete-buffer . nil)
    (counsel-git-grep-function . nil)
    (Man-goto-section . nil)
    (org-refile . nil)
    (t . string-lessp))
  "An alist of sorting functions for each collection function.
Interactive functions that call completion fit in here as well.

Nil means no sorting, which is useful to turn off the sorting for
functions that have candidates in the natural buffer order, like
`org-refile' or `Man-goto-section'.

A list can be used to associate multiple sorting functions with a
collection.  The car of the list is the current sort
function.  This list can be rotated with `ivy-rotate-sort'.

The entry associated with t is used for all fall-through cases.

See also `ivy-sort-max-size'."
  :type
  '(alist
    :key-type (choice
               (const :tag "Fall-through" t)
               (symbol :tag "Collection"))
    :value-type (choice
                 (const :tag "Plain sort" string-lessp)
                 (const :tag "File sort" ivy-sort-file-function-default)
                 (const :tag "No sort" nil)
                 (function :tag "Custom function")
                 (repeat (function :tag "Custom function"))))
  :group 'ivy)

(defun ivy--sort-function (collection)
  "Retrieve sort function for COLLECTION from `ivy-sort-functions-alist'."
  (cdr (assoc collection ivy-sort-functions-alist)))

(defun ivy-rotate-sort ()
  "Rotate through sorting functions available for current collection.
This only has an effect if multiple sorting functions are
specified for the current collection in
`ivy-sort-functions-alist'."
  (interactive)
  (let ((cell (assoc (ivy-state-collection ivy-last) ivy-sort-functions-alist)))
    (when (consp (cdr cell))
      (setcdr cell `(,@(cddr cell) ,(cadr cell)))
      (ivy--reset-state ivy-last))))

(defvar ivy-index-functions-alist
  '((swiper . ivy-recompute-index-swiper)
    (swiper-multi . ivy-recompute-index-swiper)
    (counsel-git-grep . ivy-recompute-index-swiper)
    (counsel-grep . ivy-recompute-index-swiper-async)
    (t . ivy-recompute-index-zero))
  "An alist of index recomputing functions for each collection function.
When the input changes, the appropriate function returns an
integer - the index of the matched candidate that should be
selected.")

(defvar ivy-re-builders-alist
  '((t . ivy--regex-plus))
  "An alist of regex building functions for each collection function.

Each key is (in order of priority):
1. The actual collection function, e.g. `read-file-name-internal'.
2. The symbol passed by :caller into `ivy-read'.
3. `this-command'.
4. t.

Each value is a function that should take a string and return a
valid regex or a regex sequence (see below).

Possible choices: `ivy--regex', `regexp-quote',
`ivy--regex-plus', `ivy--regex-fuzzy'.

If a function returns a list, it should format like this:
'((\"matching-regexp\" . t) (\"non-matching-regexp\") ...).

The matches will be filtered in a sequence, you can mix the
regexps that should match and that should not match as you
like.")

(defvar ivy-highlight-functions-alist
  '((ivy--regex-ignore-order . ivy--highlight-ignore-order)
    (ivy--regex-fuzzy . ivy--highlight-fuzzy)
    (ivy--regex-plus . ivy--highlight-default))
  "An alist of highlighting functions for each regex buidler function.")

(defvar ivy-initial-inputs-alist
  '((org-refile . "^")
    (org-agenda-refile . "^")
    (org-capture-refile . "^")
    (counsel-M-x . "^")
    (counsel-describe-function . "^")
    (counsel-describe-variable . "^")
    (Man-completion-table . "^")
    (woman . "^"))
  "Command to initial input table.")

(defcustom ivy-sort-max-size 30000
  "Sorting won't be done for collections larger than this."
  :type 'integer)

(defun ivy--sorted-files (dir)
  "Return the list of files in DIR.
Directories come first."
  (let* ((default-directory dir)
         (predicate (ivy-state-predicate ivy-last))
         (seq (condition-case nil
                  (all-completions "" 'read-file-name-internal)
                (error
                 (directory-files dir))))
         sort-fn)
    (if (equal dir "/")
        seq
      (setq seq (delete "./" (delete "../" seq)))
      (when (eq (setq sort-fn (ivy--sort-function 'read-file-name-internal))
                #'ivy-sort-file-function-default)
        (setq seq (mapcar (lambda (x)
                            (propertize x 'dirp (string-match-p "/\\'" x)))
                          seq)))
      (when sort-fn
        (setq seq (cl-sort seq sort-fn)))
      (dolist (dir ivy-extra-directories)
        (push dir seq))
      (if predicate
          (cl-remove-if-not predicate seq)
        seq))))

(defvar ivy-auto-select-single-candidate nil
  "When non-nil, auto-select the candidate if it is the only one.
When t, it is the same as if the user were prompted and selected the candidate
by calling the default action.  This variable has no use unless the collection
contains a single candidate.")

;;** Entry Point
;;;###autoload
(cl-defun ivy-read (prompt collection
                    &key
                      predicate require-match initial-input
                      history preselect def keymap update-fn sort
                      action unwind re-builder matcher
                      dynamic-collection caller)
  "Read a string in the minibuffer, with completion.

PROMPT is a format string, normally ending in a colon and a
space; %d anywhere in the string is replaced by the current
number of matching candidates.  For the literal % character,
escape it with %%. See also `ivy-count-format'.

COLLECTION is either a list of strings, a function, an alist, or
a hash table.

PREDICATE is applied to filter out the COLLECTION immediately.
This argument is for `completing-read' compat.

When REQUIRE-MATCH is non-nil, only members of COLLECTION can be
selected, i.e. custom text.

If INITIAL-INPUT is not nil, then insert that input in the
minibuffer initially.

HISTORY is a name of a variable to hold the completion session
history.

KEYMAP is composed with `ivy-minibuffer-map'.

If PRESELECT is not nil, then select the corresponding candidate
out of the ones that match the INITIAL-INPUT.

DEF is for compatibility with `completing-read'.

UPDATE-FN is called each time the current candidate(s) is changed.

When SORT is t, use `ivy-sort-functions-alist' for sorting.

ACTION is a lambda function to call after selecting a result.  It
takes a single string argument.

UNWIND is a lambda function to call before exiting.

RE-BUILDER is a lambda function to call to transform text into a
regex pattern.

MATCHER is to override matching.

DYNAMIC-COLLECTION is a boolean to specify if the list of
candidates is updated after each input by calling COLLECTION.

CALLER is a symbol to uniquely identify the caller to `ivy-read'.
It is used, along with COLLECTION, to determine which
customizations apply to the current completion session."
  (let ((extra-actions (delete-dups
                        (append (plist-get ivy--actions-list t)
                                (plist-get ivy--actions-list this-command)
                                (plist-get ivy--actions-list caller)))))
    (when extra-actions
      (setq action
            (cond ((functionp action)
                   `(1
                     ("o" ,action "default")
                     ,@extra-actions))
                  ((null action)
                   `(1
                     ("o" identity "default")
                     ,@extra-actions))
                  (t
                   (delete-dups (append action extra-actions)))))))
  (let ((extra-sources (plist-get ivy--sources-list caller)))
    (if extra-sources
        (progn
          (setq ivy--extra-candidates nil)
          (dolist (source extra-sources)
            (cond ((equal source '(original-source))
                   (setq ivy--extra-candidates
                         (cons source ivy--extra-candidates)))
                  ((null (cdr source))
                   (setq ivy--extra-candidates
                         (cons
                          (list (car source) (funcall (car source)))
                          ivy--extra-candidates))))))
      (setq ivy--extra-candidates '((original-source)))))
  (let ((ivy-recursive-last (and (active-minibuffer-window) ivy-last))
        (transformer-fn
         (plist-get ivy--display-transformers-list
                    (or caller (and (functionp collection)
                                    collection))))
        (ivy-display-function
         (unless (window-minibuffer-p)
           (cdr (assoc caller ivy-display-functions-alist)))))
    (setq ivy-last
          (make-ivy-state
           :prompt prompt
           :collection collection
           :predicate predicate
           :require-match require-match
           :initial-input initial-input
           :history history
           :preselect preselect
           :keymap keymap
           :update-fn update-fn
           :sort sort
           :action action
           :frame (selected-frame)
           :window (selected-window)
           :buffer (current-buffer)
           :unwind unwind
           :re-builder re-builder
           :matcher matcher
           :dynamic-collection dynamic-collection
           :display-transformer-fn transformer-fn
           :directory default-directory
           :caller caller
           :def def))
    (ivy--reset-state ivy-last)
    (prog1
        (unwind-protect
             (minibuffer-with-setup-hook
                 #'ivy--minibuffer-setup
               (let* ((hist (or history 'ivy-history))
                      (minibuffer-completion-table collection)
                      (minibuffer-completion-predicate predicate)
                      (resize-mini-windows
                       (cond
                         ((display-graphic-p) nil)
                         ((null resize-mini-windows) 'grow-only)
                         (t resize-mini-windows))))
                 (if (and ivy-auto-select-single-candidate
                          (= (length ivy--all-candidates) 1))
                     (progn
                       (setf (ivy-state-current ivy-last)
                             (car ivy--all-candidates))
                       (setq ivy-exit 'done))
                   (read-from-minibuffer
                    prompt
                    (ivy-state-initial-input ivy-last)
                    (make-composed-keymap keymap ivy-minibuffer-map)
                    nil
                    hist))
                 (when (eq ivy-exit 'done)
                   (let ((item (if ivy--directory
                                   (ivy-state-current ivy-last)
                                 ivy-text)))
                     (unless (equal item "")
                       (set hist (cons (propertize item 'ivy-index ivy--index)
                                       (delete item
                                               (cdr (symbol-value hist))))))))
                 (ivy-state-current ivy-last)))
          (remove-hook 'post-command-hook #'ivy--exhibit)
          (when (eq ivy-display-function 'ivy-display-function-overlay)
            (ivy-overlay-cleanup))
          (when (setq unwind (ivy-state-unwind ivy-last))
            (funcall unwind))
          (unless (eq ivy-exit 'done)
            (ivy-recursive-restore)))
      (ivy-call)
      (when (> (length (ivy-state-current ivy-last)) 0)
        (remove-text-properties 0 1 '(idx) (ivy-state-current ivy-last))))))

(defun ivy--reset-state (state)
  "Reset the ivy to STATE.
This is useful for recursive `ivy-read'."
  (unless (equal (selected-frame) (ivy-state-frame state))
    (select-window (active-minibuffer-window)))
  (let ((prompt (or (ivy-state-prompt state) ""))
        (collection (ivy-state-collection state))
        (predicate (ivy-state-predicate state))
        (history (ivy-state-history state))
        (preselect (ivy-state-preselect state))
        (sort (ivy-state-sort state))
        (re-builder (ivy-state-re-builder state))
        (dynamic-collection (ivy-state-dynamic-collection state))
        (initial-input (ivy-state-initial-input state))
        (require-match (ivy-state-require-match state))
        (caller (ivy-state-caller state))
        (def (ivy-state-def state)))
    (unless initial-input
      (setq initial-input (cdr (assoc (or caller this-command)
                                      ivy-initial-inputs-alist))))
    (setq ivy--directory nil)
    (setq ivy-case-fold-search ivy-case-fold-search-default)
    (setq ivy--regex-function
          (or re-builder
              (and (functionp collection)
                   (cdr (assoc collection ivy-re-builders-alist)))
              (and caller
                   (cdr (assoc caller ivy-re-builders-alist)))
              (cdr (assoc this-command ivy-re-builders-alist))
              (cdr (assoc t ivy-re-builders-alist))
              'ivy--regex))
    (setq ivy--subexps 0)
    (setq ivy--regexp-quote 'regexp-quote)
    (setq ivy--old-text "")
    (setq ivy--full-length nil)
    (setq ivy-text "")
    (setq ivy--index 0)
    (setq ivy-calling nil)
    (setq ivy-use-ignore ivy-use-ignore-default)
    (setq ivy--highlight-function
          (or (cdr (assoc ivy--regex-function ivy-highlight-functions-alist))
              #'ivy--highlight-default))
    (let (coll sort-fn)
      (cond ((eq collection 'Info-read-node-name-1)
             (if (equal Info-current-file "dir")
                 (setq coll
                       (mapcar (lambda (x) (format "(%s)" x))
                               (cl-delete-duplicates
                                (all-completions "(" collection predicate)
                                :test #'equal)))
               (setq coll (all-completions "" collection predicate))))
            ((eq collection 'read-file-name-internal)
             (setq ivy--directory default-directory)
             (when (and initial-input
                        (not (equal initial-input "")))
               (cond ((file-directory-p initial-input)
                      (when (equal (file-name-nondirectory initial-input) "")
                        (setf (ivy-state-preselect state) (setq preselect nil))
                        (setf (ivy-state-def state) (setq def nil)))
                      (setq ivy--directory initial-input)
                      (setq initial-input nil)
                      (when preselect
                        (let ((preselect-directory
                               (file-name-directory preselect)))
                          (when (and preselect-directory
                                     (not (equal
                                           (expand-file-name
                                            preselect-directory)
                                           (expand-file-name ivy--directory))))
                            (setf (ivy-state-preselect state)
                                  (setq preselect nil))))))
                     ((ignore-errors
                        (file-exists-p (file-name-directory initial-input)))
                      (setq ivy--directory (file-name-directory initial-input))
                      (setf (ivy-state-preselect state)
                            (file-name-nondirectory initial-input)))))
             (require 'dired)
             (when preselect
               (let ((preselect-directory (file-name-directory preselect)))
                 (unless (or (null preselect-directory)
                             (string= preselect-directory
                                      default-directory))
                   (setq ivy--directory preselect-directory))
                 (setf
                  (ivy-state-preselect state)
                  (setq preselect (file-name-nondirectory preselect)))))
             (setq coll (ivy--sorted-files ivy--directory))
             (when initial-input
               (unless (or require-match
                           (equal initial-input default-directory)
                           (equal initial-input ""))
                 (setq coll (cons initial-input coll)))
               (unless (and (ivy-state-action ivy-last)
                            (not (equal (ivy--get-action ivy-last) 'identity)))
                 (setq initial-input nil))))
            ((eq collection 'internal-complete-buffer)
             (setq coll (ivy--buffer-list "" ivy-use-virtual-buffers predicate)))
            (dynamic-collection
             (setq coll (funcall collection ivy-text)))
            ((and (consp collection) (listp (car collection)))
             (if (and sort (setq sort-fn (ivy--sort-function caller)))
                 (progn
                   (setq sort nil)
                   (setq coll (mapcar #'car
                                      (cl-sort
                                       (copy-sequence collection)
                                       sort-fn))))
               (setq collection
                     (setf (ivy-state-collection ivy-last)
                           (cl-remove-if-not predicate collection)))
               (setq coll (all-completions "" collection)))
             (let ((i 0))
               (ignore-errors
                 ;; cm can be read-only
                 (dolist (cm coll)
                   (add-text-properties 0 1 `(idx ,i) cm)
                   (cl-incf i)))))
            ((or (functionp collection)
                 (byte-code-function-p collection)
                 (vectorp collection)
                 (hash-table-p collection)
                 (and (listp collection) (symbolp (car collection))))
             (setq coll (all-completions "" collection predicate)))
            (t
             (setq coll collection)))
      (unless (ivy-state-dynamic-collection ivy-last)
        (setq coll (delete "" coll)))
      (when def
        (cond ((listp def)
               (setq coll (cl-union def coll :test 'equal)))
              ((member def coll))
              (t
               (push def coll))))
      (when sort
        (if (and (functionp collection)
                 (setq sort-fn (ivy--sort-function collection)))
            (when (not (eq collection 'read-file-name-internal))
              (setq coll (cl-sort coll sort-fn)))
          (when (and (not (eq history 'org-refile-history))
                     (<= (length coll) ivy-sort-max-size)
                     (setq sort-fn (ivy--sort-function caller)))
            (setq coll (cl-sort (copy-sequence coll) sort-fn)))))
      (setq coll (ivy--set-candidates coll))
      (setq ivy--old-re nil)
      (setq ivy--old-cands nil)
      (when (integerp preselect)
        (setq ivy--old-re "")
        (ivy-set-index preselect))
      (when initial-input
        ;; Needed for anchor to work
        (setq ivy--old-cands coll)
        (setq ivy--old-cands (ivy--filter initial-input coll)))
      (setq ivy--all-candidates coll)
      (unless (integerp preselect)
        (ivy-set-index (or
                        (and dynamic-collection
                             ivy--index)
                        (and preselect
                             (ivy--preselect-index
                              preselect
                              (if initial-input
                                  ivy--old-cands
                                coll)))
                        0))))
    (setq ivy-exit nil)
    (setq ivy--default
          (if (region-active-p)
              (buffer-substring
               (region-beginning)
               (region-end))
            (ivy-thing-at-point)))
    (setq ivy--prompt (ivy-add-prompt-count prompt))
    (setf (ivy-state-initial-input ivy-last) initial-input)))

(defun ivy-add-prompt-count (prompt)
  "Add count information to PROMPT."
  (cond ((string-match "%.*d" prompt)
         prompt)
        ((null ivy-count-format)
         (error
          "`ivy-count-format' can't be nil.  Set it to \"\" instead"))
        ((string-match "%d.*%d" ivy-count-format)
         (let ((w (length (number-to-string
                           (length ivy--all-candidates))))
               (s (copy-sequence ivy-count-format)))
           (string-match "%d" s)
           (match-end 0)
           (string-match "%d" s (match-end 0))
           (setq s (replace-match (format "%%-%dd" w) nil nil s))
           (string-match "%d" s)
           (concat (replace-match (format "%%%dd" w) nil nil s)
                   prompt)))
        ((string-match "%.*d" ivy-count-format)
         (concat ivy-count-format prompt))
        (ivy--directory
         prompt)
        (t
         prompt)))

;;;###autoload
(defun ivy-completing-read (prompt collection
                            &optional predicate require-match initial-input
                              history def inherit-input-method)
  "Read a string in the minibuffer, with completion.

This interface conforms to `completing-read' and can be used for
`completing-read-function'.

PROMPT is a string that normally ends in a colon and a space.
COLLECTION is either a list of strings, an alist, an obarray, or a hash table.
PREDICATE limits completion to a subset of COLLECTION.
REQUIRE-MATCH is a boolean value.  See `completing-read'.
INITIAL-INPUT is a string inserted into the minibuffer initially.
HISTORY is a list of previously selected inputs.
DEF is the default value.
INHERIT-INPUT-METHOD is currently ignored."
  (let ((handler
         (when (< ivy-completing-read-ignore-handlers-depth (minibuffer-depth))
           (assoc this-command ivy-completing-read-handlers-alist))))
    (if handler
        (let ((completion-in-region-function #'completion--in-region)
              (ivy-completing-read-ignore-handlers-depth (1+ (minibuffer-depth))))
          (funcall (cdr handler)
                   prompt collection
                   predicate require-match
                   initial-input history
                   def inherit-input-method))
      ;; See the doc of `completing-read'.
      (when (consp history)
        (when (numberp (cdr history))
          (setq initial-input (nth (1- (cdr history))
                                   (symbol-value (car history)))))
        (setq history (car history)))
      (ivy-read (replace-regexp-in-string "%" "%%" prompt)
                collection
                :predicate predicate
                :require-match (and collection require-match)
                :initial-input (cond ((consp initial-input)
                                      (car initial-input))
                                     ((and (stringp initial-input)
                                           (not (eq collection 'read-file-name-internal))
                                           (string-match "\\+" initial-input))
                                      (replace-regexp-in-string
                                       "\\+" "\\\\+" initial-input))
                                     (t
                                      initial-input))
                :preselect (if (listp def) (car def) def)
                :def (if (listp def) (car def) def)
                :history history
                :keymap nil
                :sort t
                :caller (cond ((called-interactively-p 'any)
                               this-command)
                              ((and collection (symbolp collection))
                               collection))))))

(defun ivy-completing-read-with-empty-string-def
    (prompt collection
            &optional predicate require-match initial-input
            history def inherit-input-method)
  "Same as `ivy-completing-read' but with different handling of DEF.

Specifically, if DEF is nil, it is treated the same as if DEF was
the empty string. This mimics the behavior of
`completing-read-default'. This function can therefore be used in
place of `ivy-completing-read' for commands that rely on this
behavior."
  (ivy-completing-read
   prompt collection predicate require-match initial-input
   history (or def "") inherit-input-method))

(declare-function mc/all-fake-cursors "ext:multiple-cursors-core")

(defun ivy-completion-in-region-action (str)
  "Insert STR, erasing the previous one.
The previous string is between `ivy-completion-beg' and `ivy-completion-end'."
  (when (consp str)
    (setq str (cdr str)))
  (when (stringp str)
    (let ((fake-cursors (and (featurep 'multiple-cursors)
                             (mc/all-fake-cursors)))
          (pt (point))
          (beg ivy-completion-beg)
          (end ivy-completion-end))
      (when ivy-completion-beg
        (delete-region
         ivy-completion-beg
         ivy-completion-end))
      (setq ivy-completion-beg
            (move-marker (make-marker) (point)))
      (insert (substring-no-properties str))
      (setq ivy-completion-end
            (move-marker (make-marker) (point)))
      (save-excursion
        (dolist (cursor fake-cursors)
          (goto-char (overlay-start cursor))
          (delete-region (+ (point) (- beg pt))
                         (+ (point) (- end pt)))
          (insert (substring-no-properties str))
          ;; manually move the fake cursor
          (move-overlay cursor (point) (1+ (point)))
          (move-marker (overlay-get cursor 'point) (point))
          (move-marker (overlay-get cursor 'mark) (point)))))))

(defun ivy-completion-common-length (str)
  "Return the length of the first `completions-common-part' face in STR."
  (let ((pos 0)
        (len (length str))
        face-sym)
    (while (and (<= pos len)
                (let ((prop (or (prog1 (get-text-property
                                        pos 'face str)
                                  (setq face-sym 'face))
                                (prog1 (get-text-property
                                        pos 'font-lock-face str)
                                  (setq face-sym 'font-lock-face)))))
                  (not (eq 'completions-common-part
                           (if (listp prop) (car prop) prop)))))
      (setq pos (1+ pos)))
    (if (< pos len)
        (or (next-single-property-change pos face-sym str) len)
      0)))

(defun ivy-completion-in-region (start end collection &optional predicate)
  "An Ivy function suitable for `completion-in-region-function'.
The function completes the text between START and END using COLLECTION.
PREDICATE (a function called with no arguments) says when to exit.
See `completion-in-region' for further information."
  (let* ((enable-recursive-minibuffers t)
         (str (buffer-substring-no-properties start end))
         (completion-ignore-case case-fold-search)
         (comps
          (completion-all-completions str collection predicate (- end start)))
         (ivy--prompts-list (if (window-minibuffer-p)
                                ivy--prompts-list
                              '(ivy-completion-in-region (lambda nil)))))
    (if (null comps)
        (message "No matches")
      (let ((len (min (ivy-completion-common-length (car comps))
                      (length str))))
        (nconc comps nil)
        (setq ivy-completion-beg (- end len))
        (setq ivy-completion-end end)
        (if (null (cdr comps))
            (if (string= str (car comps))
                (message "Sole match")
              (unless (minibuffer-window-active-p (selected-window))
                (setf (ivy-state-window ivy-last) (selected-window)))
              (ivy-completion-in-region-action
               (substring-no-properties
                (car comps))))
          (let* ((w (1+ (floor (log (length comps) 10))))
                 (ivy-count-format (if (string= ivy-count-format "")
                                       ivy-count-format
                                     (format "%%-%dd " w)))
                 (prompt (format "(%s): " str)))
            (and
             (ivy-read (if (string= ivy-count-format "")
                           prompt
                         (replace-regexp-in-string "%" "%%" prompt))
                       ;; remove 'completions-first-difference face
                       (mapcar #'substring-no-properties comps)
                       :predicate predicate
                       :action #'ivy-completion-in-region-action
                       :caller 'ivy-completion-in-region)
             t)))))))

(defcustom ivy-do-completion-in-region t
  "When non-nil `ivy-mode' will set `completion-in-region-function'."
  :type 'boolean)

;;;###autoload
(define-minor-mode ivy-mode
  "Toggle Ivy mode on or off.
Turn Ivy mode on if ARG is positive, off otherwise.
Turning on Ivy mode sets `completing-read-function' to
`ivy-completing-read'.

Global bindings:
\\{ivy-mode-map}

Minibuffer bindings:
\\{ivy-minibuffer-map}"
  :group 'ivy
  :global t
  :keymap ivy-mode-map
  :lighter " ivy"
  (if ivy-mode
      (progn
        (setq completing-read-function 'ivy-completing-read)
        (when ivy-do-completion-in-region
          (setq completion-in-region-function 'ivy-completion-in-region)))
    (setq completing-read-function 'completing-read-default)
    (setq completion-in-region-function 'completion--in-region)))

(defun ivy--preselect-index (preselect candidates)
  "Return the index of PRESELECT in CANDIDATES."
  (cond ((integerp preselect)
         preselect)
        ((cl-position preselect candidates :test #'equal))
        ((stringp preselect)
         (let ((re preselect))
           (cl-position-if
            (lambda (x)
              (string-match re x))
            candidates)))))

;;* Implementation
;;** Regex
(defun ivy-re-match (re-seq str)
  "Return non-nil if RE-SEQ is matched by STR.

RE-SEQ is a list of (RE . MATCH-P).

RE is a regular expression.

MATCH-P is t when RE should match STR and nil when RE should not
match STR.

Each element of RE-SEQ must match for the function to return true.

This concept is used to generalize regular expressions for
`ivy--regex-plus' and `ivy--regex-ignore-order'."
  (let ((res t)
        re)
    (while (and res (setq re (pop re-seq)))
      (setq res
            (if (cdr re)
                (string-match-p (car re) str)
              (not (string-match-p (car re) str)))))
    res))

(defvar ivy--regex-hash
  (make-hash-table :test #'equal)
  "Store pre-computed regex.")

(defun ivy--split (str)
  "Split STR into a list by single spaces.
The remaining spaces stick to their left.
This allows to \"quote\" N spaces by inputting N+1 spaces."
  (let ((len (length str))
        start0
        (start1 0)
        res s
        match-len)
    (while (and (string-match " +" str start1)
                (< start1 len))
      (if (and (> (match-beginning 0) 2)
               (string= "[^" (substring
                              str
                              (- (match-beginning 0) 2)
                              (match-beginning 0))))
          (progn
            (setq start1 (match-end 0))
            (setq start0 0))
        (setq match-len (- (match-end 0) (match-beginning 0)))
        (if (= match-len 1)
            (progn
              (when start0
                (setq start1 start0)
                (setq start0 nil))
              (push (substring str start1 (match-beginning 0)) res)
              (setq start1 (match-end 0)))
          (setq str (replace-match
                     (make-string (1- match-len) ?\ )
                     nil nil str))
          (setq start0 (or start0 start1))
          (setq start1 (1- (match-end 0))))))
    (if start0
        (push (substring str start0) res)
      (setq s (substring str start1))
      (unless (= (length s) 0)
        (push s res)))
    (nreverse res)))

(defun ivy--regex (str &optional greedy)
  "Re-build regex pattern from STR in case it has a space.
When GREEDY is non-nil, join words in a greedy way."
  (let ((hashed (unless greedy
                  (gethash str ivy--regex-hash))))
    (if hashed
        (prog1 (cdr hashed)
          (setq ivy--subexps (car hashed)))
      (when (string-match "\\([^\\]\\|^\\)\\\\$" str)
        (setq str (substring str 0 -1)))
      (cdr (puthash str
                    (let ((subs (ivy--split str)))
                      (if (= (length subs) 1)
                          (cons
                           (setq ivy--subexps 0)
                           (car subs))
                        (cons
                         (setq ivy--subexps (length subs))
                         (mapconcat
                          (lambda (x)
                            (if (string-match "\\`\\\\([^?].*\\\\)\\'" x)
                                x
                              (format "\\(%s\\)" x)))
                          subs
                          (if greedy
                              ".*"
                            ".*?")))))
                    ivy--regex-hash)))))

(defun ivy--legal-regex-p (str)
  "Return t if STR is valid regular expression."
  (condition-case nil
      (progn
        (string-match-p str "")
        t)
    (invalid-regexp nil)))

(defun ivy--regex-or-literal (str)
  "If STR isn't a legal regex, escape it."
  (if (ivy--legal-regex-p str) str (regexp-quote str)))

(defun ivy--split-negation (str)
  "Split STR into text before and after !.
Don't split if it's escaped with \\!.

Assumes there is at most one unescaped !."
  (let (parts
        (part ""))
    (mapc
     (lambda (char)
       (let ((prev-char (if (zerop (length part))
                            nil
                          (elt part (1- (length part))))))
         ;; Split on !, unless it's escaped.
         (cond
          ;; Store "\!" as "!".
          ((and (eq char ?!) (eq prev-char ?\\))
           (setq part (concat (substring part 0 (1- (length part)))
                              "!")))
          ;; Split on "!".
          ((eq char ?!)
           (push part parts)
           (setq part ""))
          ;; Otherwise, append the current character.
          (t
           (setq part (concat part (string char)))))))
     str)
    (unless (zerop (length part))
      (push part parts))
    (setq parts (nreverse parts))
    ;; If we have more than unescaped !, just discard the extra parts
    ;; rather than crashing. We can't warn or error because the
    ;; minibuffer is already active.
    (when (> (length parts) 2)
      (setq parts (list (cl-first parts) (cl-second parts))))
    parts))

(defun ivy--split-spaces (str)
  "Split STR on spaces, unless they're preceded by \\.
No unescaped spaces are present in the output."
  (let (parts
        (part ""))
    (mapc
     (lambda (char)
       (let ((prev-char (if (zerop (length part))
                            nil
                          (elt part (1- (length part))))))
         (cond
          ;; Store "\ " as " ".
          ((and (eq char ?\s) (eq prev-char ?\\))
           (setq part (concat (substring part 0 (1- (length part)))
                              " ")))
          ;; Split on " ".
          ((eq char ?\s)
           (unless (zerop (length part))
             (push part parts))
           (setq part ""))
          ;; Otherwise, append the current character.
          (t
           (setq part (concat part (string char)))))))
     str)
    (unless (zerop (length part))
      (push part parts))
    (nreverse parts)))

(defun ivy--regex-ignore-order (str)
  "Re-build regex from STR by splitting at spaces and using ! for negation.

Examples:
foo          -> matches \"foo\"
foo bar      -> matches if both \"foo\" and \"bar\" match (any order)
foo !bar     -> matches if \"foo\" matches and \"bar\" does not match
foo !bar baz -> matches if \"foo\" matches and neither \"bar\" nor \"baz\" match
foo[a-z]     -> matches \"foo[a-z]\"

Escaping examples:
foo\!bar -> matches \"foo!bar\"
foo\ bar -> matches \"foo bar\"

If STR isn't a valid input, fall back to exact matching:
foo[     -> matches \"foo\[\" (invalid regex, so literal [ character)

Returns a list suitable for `ivy-re-match'."
  (let* (regex-parts
         (raw-parts (ivy--split-negation str)))
    (dolist (part (ivy--split-spaces (car raw-parts)))
      (push (cons (ivy--regex-or-literal part) t) regex-parts))
    (when (cdr raw-parts)
      (dolist (part (ivy--split-spaces (cadr raw-parts)))
        (push (cons (ivy--regex-or-literal part) nil) regex-parts)))
    (if regex-parts (nreverse regex-parts)
      "")))

(defun ivy--regex-plus (str)
  "Build a regex sequence from STR.
Spaces are wild card characters, everything before \"!\" should
match.  Everything after \"!\" should not match."
  (let ((parts (split-string str "!" t)))
    (cl-case (length parts)
      (0
       "")
      (1
       (if (string= (substring str 0 1) "!")
           (list (cons "" t)
                 (list (ivy--regex (car parts))))
         (ivy--regex (car parts))))
      (2
       (cons
        (cons (ivy--regex (car parts)) t)
        (mapcar #'list (split-string (cadr parts) " " t))))
      (t (error "Unexpected: use only one !")))))

(defun ivy--regex-fuzzy (str)
  "Build a regex sequence from STR.
Insert .* between each char."
  (if (string-match "\\`\\(\\^?\\)\\(.*?\\)\\(\\$?\\)\\'" str)
      (prog1
          (concat (match-string 1 str)
                  (mapconcat
                   (lambda (x)
                     (format "\\(%c\\)" x))
                   (string-to-list (match-string 2 str))
                   ".*?")
                  (match-string 3 str))
        (setq ivy--subexps (length (match-string 2 str))))
    str))

(defcustom ivy-fixed-height-minibuffer nil
  "When non nil, fix the height of the minibuffer during ivy completion.
This effectively sets the minimum height at this level to `ivy-height' and
tries to ensure that it does not change depending on the number of candidates."
  :group 'ivy
  :type 'boolean)

;;** Rest
(defcustom ivy-truncate-lines t
  "Minibuffer setting for `truncate-lines'."
  :type 'boolean)

(defun ivy--minibuffer-setup ()
  "Setup ivy completion in the minibuffer."
  (set (make-local-variable 'completion-show-inline-help) nil)
  (set (make-local-variable 'minibuffer-default-add-function)
       (lambda ()
         (list ivy--default)))
  (set (make-local-variable 'inhibit-field-text-motion) nil)
  (when (display-graphic-p)
    (setq truncate-lines ivy-truncate-lines))
  (setq-local max-mini-window-height ivy-height)
  (when (and ivy-fixed-height-minibuffer
             (not (eq (ivy-state-caller ivy-last) 'ivy-completion-in-region)))
    (set-window-text-height (selected-window)
                            (+ ivy-height
                               (if ivy-add-newline-after-prompt
                                   1
                                 0))))
  (add-hook 'post-command-hook #'ivy--exhibit nil t)
  ;; show completions with empty input
  (ivy--exhibit))

(defun ivy--input ()
  "Return the current minibuffer input."
  ;; assume one-line minibuffer input
  (buffer-substring-no-properties
   (minibuffer-prompt-end)
   (line-end-position)))

(defun ivy--cleanup ()
  "Delete the displayed completion candidates."
  (save-excursion
    (goto-char (minibuffer-prompt-end))
    (delete-region (line-end-position) (point-max))))

(defun ivy-cleanup-string (str)
  "Remove unwanted text properties from STR."
  (remove-text-properties 0 (length str) '(field) str)
  str)

(defvar ivy-set-prompt-text-properties-function
  'ivy-set-prompt-text-properties-default
  "Function to set the text properties of the default ivy prompt.
Called with two arguments, PROMPT and STD-PROPS.
The returned value should be the updated PROMPT.")

(defun ivy-set-prompt-text-properties-default (prompt std-props)
  "Set text properties of PROMPT.
STD-PROPS is a property list containing the default text properties."
  (ivy--set-match-props prompt "confirm"
                        `(face ivy-confirm-face ,@std-props))
  (ivy--set-match-props prompt "match required"
                        `(face ivy-match-required-face ,@std-props))
  prompt)

(defun ivy-prompt ()
  "Return the current prompt."
  (let ((fn (plist-get ivy--prompts-list (ivy-state-caller ivy-last))))
    (if fn
        (condition-case nil
            (funcall fn)
          (error
           (warn "`counsel-prompt-function' should take 0 args")
           ;; old behavior
           (funcall fn (ivy-state-prompt ivy-last))))
      ivy--prompt)))

(defun ivy--insert-prompt ()
  "Update the prompt according to `ivy--prompt'."
  (when (setq ivy--prompt (ivy-prompt))
    (unless (memq this-command '(ivy-done ivy-alt-done ivy-partial-or-done
                                 counsel-find-symbol))
      (setq ivy--prompt-extra ""))
    (let (head tail)
      (if (string-match "\\(.*?\\)\\(:? ?\\)\\'" ivy--prompt)
          (progn
            (setq head (match-string 1 ivy--prompt))
            (setq tail (match-string 2 ivy--prompt)))
        (setq head ivy--prompt)
        (setq tail ""))
      (let ((inhibit-read-only t)
            (std-props '(front-sticky t rear-nonsticky t field t read-only t))
            (n-str
             (concat
              (if (and (bound-and-true-p minibuffer-depth-indicate-mode)
                       (> (minibuffer-depth) 1))
                  (format "[%d] " (minibuffer-depth))
                "")
              (concat
               (if (string-match "%d.*%d" ivy-count-format)
                   (format head
                           (1+ ivy--index)
                           (or (and (ivy-state-dynamic-collection ivy-last)
                                    ivy--full-length)
                               ivy--length))
                 (format head
                         (or (and (ivy-state-dynamic-collection ivy-last)
                                  ivy--full-length)
                             ivy--length)))
               ivy--prompt-extra
               tail)))
            (d-str (if ivy--directory
                       (abbreviate-file-name ivy--directory)
                     "")))
        (save-excursion
          (goto-char (point-min))
          (delete-region (point-min) (minibuffer-prompt-end))
          (let ((len-n (length n-str))
                (len-d (length d-str))
                (ww (window-width)))
            (setq n-str
                  (cond ((> (+ len-n len-d) ww)
                         (concat n-str "\n" d-str "\n"))
                        ((> (+ len-n len-d (length ivy-text)) ww)
                         (concat n-str d-str "\n"))
                        (t
                         (concat n-str d-str)))))
          (when ivy-add-newline-after-prompt
            (setq n-str (concat n-str "\n")))
          (let ((regex (format "\\([^\n]\\{%d\\}\\)[^\n]" (window-width))))
            (while (string-match regex n-str)
              (setq n-str (replace-match
                           (concat (match-string 1 n-str) "\n")
                           nil t n-str 1))))
          (set-text-properties 0 (length n-str)
                               `(face minibuffer-prompt ,@std-props)
                               n-str)
          (setq n-str (funcall ivy-set-prompt-text-properties-function
                               n-str std-props))
          (insert n-str))
        ;; Mark prompt as selected if the user moves there or it is the only
        ;; option left.  Since the user input stays put, we have to manually
        ;; remove the face as well.
        (when (ivy--prompt-selectable-p)
          (if (or (= ivy--index -1)
                  (= ivy--length 0))
              (add-face-text-property
               (minibuffer-prompt-end) (line-end-position) 'ivy-prompt-match)
            (remove-text-properties
             (minibuffer-prompt-end) (line-end-position) '(face))))
        ;; get out of the prompt area
        (constrain-to-field nil (point-max))))))

(defun ivy--set-match-props (str match props &optional subexp)
  "Set text properties of STR that match MATCH to PROPS.
SUBEXP is a number which specifies the regexp group to use.
If nil, the text properties are applied to the whole match."
  (when (null subexp)
    (setq subexp 0))
  (when (string-match match str)
    (set-text-properties
     (match-beginning subexp)
     (match-end subexp)
     props
     str)))

(defvar inhibit-message)

(defun ivy--sort-maybe (collection)
  "Sort COLLECTION if needed."
  (let ((sort (ivy-state-sort ivy-last)))
    (if (null sort)
        collection
      (let ((sort-fn (or (and (functionp sort) sort)
                         (ivy--sort-function (ivy-state-collection ivy-last))
                         (ivy--sort-function t))))
        (if (functionp sort-fn)
            (cl-sort (copy-sequence collection) sort-fn)
          collection)))))

(defcustom ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected
  "Action to take when a slash is added to the end of a non existing directory.
Possible choices are 'ivy-magic-slash-non-match-cd-selected,
'ivy-magic-slash-non-match-create, or nil"
  :type '(choice
          (const :tag "Use currently selected directory"
                 ivy-magic-slash-non-match-cd-selected)
          (const :tag "Create and use new directory"
                 ivy-magic-slash-non-match-create)
          (const :tag "Do nothing"
                 nil)))

(defun ivy--create-and-cd (dir)
  "When completing file names, create directory DIR and move there."
  (make-directory dir)
  (ivy--cd dir))

(defun ivy--magic-file-slash ()
  "Handle slash when completing file names."
  (when (or (and (eq this-command 'self-insert-command)
                 (eolp))
            (eq this-command 'ivy-partial-or-done))
    (cond ((member ivy-text ivy--all-candidates)
           (ivy--cd (expand-file-name ivy-text ivy--directory)))
          ((string-match "//\\'" ivy-text)
           (if (and default-directory
                    (string-match "\\`[[:alpha:]]:/" default-directory))
               (ivy--cd (match-string 0 default-directory))
             (ivy--cd "/")))
          ((string-match "\\`/ssh:" ivy-text)
           (ivy--cd (file-name-directory ivy-text)))
          ((string-match "[[:alpha:]]:/\\'" ivy-text)
           (let ((drive-root (match-string 0 ivy-text)))
             (when (file-exists-p drive-root)
               (ivy--cd drive-root))))
          ((and (file-exists-p ivy-text)
                (not (string= ivy-text "/"))
                (file-directory-p ivy-text))
           (ivy--cd (expand-file-name ivy-text)))
          ((and (or (> ivy--index 0)
                    (= ivy--length 1)
                    (not (string= ivy-text "/")))
                (let ((default-directory ivy--directory))
                  (and
                   (not (equal (ivy-state-current ivy-last) ""))
                   (file-directory-p (ivy-state-current ivy-last))
                   (file-exists-p (ivy-state-current ivy-last)))))
           (when (eq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-cd-selected)
             (ivy--cd
              (expand-file-name (ivy-state-current ivy-last) ivy--directory)))
           (when (and (eq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
                      (not (string= ivy-text "/")))
             (ivy--create-and-cd (expand-file-name ivy-text ivy--directory))))
          (t
           (when (and
                  (eq ivy-magic-slash-non-match-action 'ivy-magic-slash-non-match-create)
                  (not (string= ivy-text "/")))
             (ivy--create-and-cd (expand-file-name ivy-text ivy--directory)))))))

(defcustom ivy-magic-tilde t
  "When non-nil, ~ will move home when selecting files.
Otherwise, ~/ will move home."
  :type 'boolean)

(defun ivy--exhibit ()
  "Insert Ivy completions display.
Should be run via minibuffer `post-command-hook'."
  (when (memq 'ivy--exhibit post-command-hook)
    (let ((inhibit-field-text-motion nil))
      (constrain-to-field nil (point-max)))
    (setq ivy-text (ivy--input))
    (if (ivy-state-dynamic-collection ivy-last)
        ;; while-no-input would cause annoying
        ;; "Waiting for process to die...done" message interruptions
        (let ((inhibit-message t))
          (unless (equal ivy--old-text ivy-text)
            (while-no-input
              (setq ivy--all-candidates
                    (ivy--sort-maybe
                     (funcall (ivy-state-collection ivy-last) ivy-text)))
              (setq ivy--old-text ivy-text)))
          (when ivy--all-candidates
            (ivy--insert-minibuffer
             (ivy--format ivy--all-candidates))))
      (cond (ivy--directory
             (cond ((or (string= "~/" ivy-text)
                        (and (string= "~" ivy-text)
                             ivy-magic-tilde))
                    (ivy--cd (expand-file-name "~/")))
                   ((string-match "/\\'" ivy-text)
                    (ivy--magic-file-slash))))
            ((eq (ivy-state-collection ivy-last) 'internal-complete-buffer)
             (when (or (and (string-match "\\` " ivy-text)
                            (not (string-match "\\` " ivy--old-text)))
                       (and (string-match "\\` " ivy--old-text)
                            (not (string-match "\\` " ivy-text))))
               (setq ivy--all-candidates
                     (if (and (> (length ivy-text) 0)
                              (eq (aref ivy-text 0)
                                  ?\ ))
                         (ivy--buffer-list " ")
                       (ivy--buffer-list "" ivy-use-virtual-buffers)))
               (setq ivy--old-re nil))))
      (ivy--insert-minibuffer
       (with-current-buffer (ivy-state-buffer ivy-last)
         (ivy--format
          (ivy--filter ivy-text ivy--all-candidates))))
      (setq ivy--old-text ivy-text))))

(defun ivy--insert-minibuffer (text)
  "Insert TEXT into minibuffer with appropriate cleanup."
  (let ((resize-mini-windows nil)
        (update-fn (ivy-state-update-fn ivy-last))
        (old-mark (marker-position (mark-marker)))
        deactivate-mark)
    (ivy--cleanup)
    (when update-fn
      (funcall update-fn))
    (ivy--insert-prompt)
    ;; Do nothing if while-no-input was aborted.
    (when (stringp text)
      (if ivy-display-function
          (funcall ivy-display-function text)
        (let ((buffer-undo-list t))
          (save-excursion
            (forward-line 1)
            (insert text)))))
    (when (display-graphic-p)
      (ivy--resize-minibuffer-to-fit))
    ;; prevent region growing due to text remove/add
    (when (region-active-p)
      (set-mark old-mark))))

(defun ivy--resize-minibuffer-to-fit ()
  "Resize the minibuffer window size to fit the text in the minibuffer."
  (unless (frame-root-window-p (minibuffer-window))
    (with-selected-window (minibuffer-window)
      (if (fboundp 'window-text-pixel-size)
          (let ((text-height (cdr (window-text-pixel-size)))
                (body-height (window-body-height nil t)))
            (when (> text-height body-height)
              ;; Note: the size increment needs to be at least
              ;; frame-char-height, otherwise resizing won't do
              ;; anything.
              (let ((delta (max (- text-height body-height)
                                (frame-char-height))))
                (window-resize nil delta nil t t))))
        (let ((text-height (count-screen-lines))
              (body-height (window-body-height)))
          (when (> text-height body-height)
            (window-resize nil (- text-height body-height) nil t)))))))

(declare-function colir-blend-face-background "ext:colir")

(defun ivy--add-face (str face)
  "Propertize STR with FACE.
`font-lock-append-text-property' is used, since it's better than
`propertize' or `add-face-text-property' in this case."
  (require 'colir)
  (condition-case nil
      (progn
        (colir-blend-face-background 0 (length str) face str)
        (let ((foreground (face-foreground face)))
          (when foreground
            (add-face-text-property
             0 (length str)
             `(:foreground ,foreground)
             nil
             str))))
    (error
     (ignore-errors
       (font-lock-append-text-property 0 (length str) 'face face str))))
  str)

(declare-function flx-make-string-cache "ext:flx")
(declare-function flx-score "ext:flx")

(defvar ivy--flx-cache nil)

(eval-after-load 'flx
  '(setq ivy--flx-cache (flx-make-string-cache)))

(defun ivy-toggle-case-fold ()
  "Toggle the case folding between nil and auto/always.

If auto, `case-fold-search' is t, when the input is all lower case,
otherwise nil.

If always, `case-fold-search' is always t, regardless of the input.

Otherwise `case-fold-search' is always nil, regardless of the input.

In any completion session, the case folding starts in
`ivy-case-fold-search-default'."
  (interactive)
  (setq ivy-case-fold-search
        (if ivy-case-fold-search
            nil
          (or ivy-case-fold-search-default 'auto)))
  ;; reset cache so that the candidate list updates
  (setq ivy--old-re nil))

(defun ivy--re-filter (re candidates)
  "Return all RE matching CANDIDATES.
RE is a list of cons cells, with a regexp car and a boolean cdr.
When the cdr is t, the car must match.
Otherwise, the car must not match."
  (let ((re-list (if (stringp re) (list (cons re t)) re))
        (res candidates))
    (dolist (re re-list)
      (setq res
            (ignore-errors
              (funcall
               (if (cdr re)
                   #'cl-remove-if-not
                 #'cl-remove-if)
               (let ((re-str (car re)))
                 (lambda (x) (string-match re-str x)))
               res))))
    res))

(defun ivy--filter (name candidates)
  "Return all items that match NAME in CANDIDATES.
CANDIDATES are assumed to be static."
  (let ((re (funcall ivy--regex-function name)))
    (if (and
         ivy--old-re
         ivy--old-cands
         (equal re ivy--old-re))
        ;; quick caching for "C-n", "C-p" etc.
        ivy--old-cands
      (let* ((re-str (if (listp re) (caar re) re))
             (matcher (ivy-state-matcher ivy-last))
             (case-fold-search
              (and ivy-case-fold-search
                   (or (eq ivy-case-fold-search 'always)
                       (string= name (downcase name)))))
             (cands (cond
                      (matcher
                       (funcall matcher re candidates))
                      ((and ivy--old-re
                            (stringp re)
                            (stringp ivy--old-re)
                            (not (string-match "\\\\" ivy--old-re))
                            (not (equal ivy--old-re ""))
                            (memq (cl-search
                                   (if (string-match "\\\\)\\'" ivy--old-re)
                                       (substring ivy--old-re 0 -2)
                                     ivy--old-re)
                                   re)
                                  '(0 2)))
                       (ignore-errors
                         (cl-remove-if-not
                          (lambda (x) (string-match re x))
                          ivy--old-cands)))
                      (t
                       (ivy--re-filter re candidates)))))
        (if (memq (cdr (assoc (ivy-state-caller ivy-last)
                              ivy-index-functions-alist))
                  '(ivy-recompute-index-swiper
                    ivy-recompute-index-swiper-async))
            (progn
              (ivy--recompute-index name re-str cands)
              (setq ivy--old-cands (ivy--sort name cands)))
          (setq ivy--old-cands (ivy--sort name cands))
          (ivy--recompute-index name re-str ivy--old-cands))
        (setq ivy--old-re re)
        ivy--old-cands))))

(defun ivy--set-candidates (x)
  "Update `ivy--all-candidates' with X."
  (let (res)
    (dolist (source ivy--extra-candidates)
      (if (equal source '(original-source))
          (if (null res)
              (setq res x)
            (setq res (append x res)))
        (setq ivy--old-re nil)
        (setq res (append
                   (ivy--filter ivy-text (cadr source))
                   res))))
    (setq ivy--all-candidates res)))

(defcustom ivy-sort-matches-functions-alist
  '((t . nil)
    (ivy-switch-buffer . ivy-sort-function-buffer))
  "An alist of functions for sorting matching candidates.

Unlike `ivy-sort-functions-alist', which is used to sort the
whole collection only once, this alist of functions are used to
sort only matching candidates after each change in input.

The alist KEY is either a collection function or t to match
previously unmatched collection functions.

The alist VAL is a sorting function with the signature of
`ivy--prefix-sort'."
  :type '(alist
          :key-type (choice
                     (const :tag "Fall-through" t)
                     (symbol :tag "Collection"))
          :value-type
          (choice
           (const :tag "Don't sort" nil)
           (const :tag "Put prefix matches ahead" 'ivy--prefix-sort)
           (function :tag "Custom sort function"))))

(defun ivy--sort-files-by-date (_name candidates)
  "Re-sort CANDIDATES according to file modification date."
  (let ((default-directory ivy--directory))
    (cl-sort (copy-sequence candidates)
             (lambda (f1 f2)
               (time-less-p
                (nth 5 (file-attributes f2))
                (nth 5 (file-attributes f1)))))))

(defvar ivy--flx-featurep (require 'flx nil 'noerror))

(defun ivy--sort (name candidates)
  "Re-sort candidates by NAME.
All CANDIDATES are assumed to match NAME."
  (let ((key (or (ivy-state-caller ivy-last)
                 (when (functionp (ivy-state-collection ivy-last))
                   (ivy-state-collection ivy-last))))
        fun)
    (cond ((and ivy--flx-featurep
                (eq ivy--regex-function 'ivy--regex-fuzzy))
           (ivy--flx-sort name candidates))
          ((setq fun (cdr (or (assoc key ivy-sort-matches-functions-alist)
                              (assoc t ivy-sort-matches-functions-alist))))
           (funcall fun name candidates))
          (t
           candidates))))

(defun ivy--prefix-sort (name candidates)
  "Re-sort candidates by NAME.
All CANDIDATES are assumed to match NAME.
Prefix matches to NAME are put ahead of the list."
  (if (or (string-match "^\\^" name) (string= name ""))
      candidates
    (let ((re-prefix (concat "^" (funcall ivy--regex-function name)))
          res-prefix
          res-noprefix)
      (dolist (s candidates)
        (if (string-match re-prefix s)
            (push s res-prefix)
          (push s res-noprefix)))
      (nconc
       (nreverse res-prefix)
       (nreverse res-noprefix)))))

(defvar ivy--virtual-buffers nil
  "Store the virtual buffers alist.")

(defun ivy-sort-function-buffer (name candidates)
  "Re-sort candidates by NAME.
CANDIDATES is a list of buffer names each containing NAME.
Sort open buffers before virtual buffers, and prefix matches
before substring matches."
  (if (or (string-match "^\\^" name) (string= name ""))
      candidates
    (let* ((base-re (funcall ivy--regex-function name))
           (base-re (if (consp base-re) (caar base-re) base-re))
           (re-prefix (concat "^\\*" base-re))
           res-prefix
           res-noprefix
           res-virtual-prefix
           res-virtual-noprefix)
      (unless (cl-find-if (lambda (s) (string-match re-prefix s)) candidates)
        (setq re-prefix (concat "^" base-re)))
      (dolist (s candidates)
        (cond
          ((and (assoc s ivy--virtual-buffers) (string-match re-prefix s))
           (push s res-virtual-prefix))
          ((assoc s ivy--virtual-buffers)
           (push s res-virtual-noprefix))
          ((string-match re-prefix s)
           (push s res-prefix))
          (t
           (push s res-noprefix))))
      (nconc
       (nreverse res-prefix)
       (nreverse res-noprefix)
       (nreverse res-virtual-prefix)
       (nreverse res-virtual-noprefix)))))

(defun ivy--recompute-index (name re-str cands)
  "Recompute index of selected candidate matching NAME.
RE-STR is the regexp, CANDS are the current candidates."
  (let* ((caller (ivy-state-caller ivy-last))
         (func (or (and caller (cdr (assoc caller ivy-index-functions-alist)))
                   (cdr (assoc t ivy-index-functions-alist))
                   #'ivy-recompute-index-zero)))
    (unless (eq this-command 'ivy-resume)
      (ivy-set-index
       (or
        (cl-position (if (and (> (length name) 0)
                              (eq ?^ (aref name 0)))
                         (substring name 1)
                       name) cands
                       :test #'equal)
        (and ivy--directory
             (cl-position
              (concat re-str "/") cands
              :test #'equal))
        (and (eq caller 'ivy-switch-buffer)
             (> (length name) 0)
             0)
        (and (not (string= name ""))
             (not (and ivy--flx-featurep
                       (eq ivy--regex-function 'ivy--regex-fuzzy)
                       (< (length cands) 200)))
             ivy--old-cands
             (cl-position (nth ivy--index ivy--old-cands)
                          cands))
        (funcall func re-str cands))))
    (when (or (string= name "")
              (string= name "^"))
      (ivy-set-index
       (or (ivy--preselect-index
            (ivy-state-preselect ivy-last)
            cands)
           ivy--index)))))

(defun ivy-recompute-index-swiper (_re-str cands)
  "Recompute index of selected candidate when using `swiper'.
CANDS are the current candidates."
  (condition-case nil
      (let ((tail (nthcdr ivy--index ivy--old-cands))
            idx)
        (if (and tail ivy--old-cands (not (equal "^" ivy--old-re)))
            (progn
              (while (and tail (null idx))
                ;; Compare with eq to handle equal duplicates in cands
                (setq idx (cl-position (pop tail) cands)))
              (or
               idx
               (1- (length cands))))
          (if ivy--old-cands
              ivy--index
            ;; already in ivy-state-buffer
            (let ((n (line-number-at-pos))
                  (res 0)
                  (i 0))
              (dolist (c cands)
                (when (eq n (read (get-text-property 0 'swiper-line-number c)))
                  (setq res i))
                (cl-incf i))
              res))))
    (error 0)))

(defun ivy-recompute-index-swiper-async (_re-str cands)
  "Recompute index of selected candidate when using `swiper' asynchronously.
CANDS are the current candidates."
  (if (null ivy--old-cands)
      (let ((ln (with-ivy-window
                  (line-number-at-pos))))
        (or
         ;; closest to current line going forwards
         (cl-position-if (lambda (x)
                           (>= (string-to-number x) ln))
                         cands)
         ;; closest to current line going backwards
         (1- (length cands))))
    (let ((tail (nthcdr ivy--index ivy--old-cands))
          idx)
      (if (and tail ivy--old-cands (not (equal "^" ivy--old-re)))
          (progn
            (while (and tail (null idx))
              ;; Compare with `equal', since the collection is re-created
              ;; each time with `split-string'
              (setq idx (cl-position (pop tail) cands :test #'equal)))
            (or idx 0))
        ivy--index))))

(defun ivy-recompute-index-zero (_re-str _cands)
  "Recompute index of selected candidate.
This function serves as a fallback when nothing else is available."
  0)

(defcustom ivy-minibuffer-faces
  '(ivy-minibuffer-match-face-1
    ivy-minibuffer-match-face-2
    ivy-minibuffer-match-face-3
    ivy-minibuffer-match-face-4)
  "List of `ivy' faces for minibuffer group matches."
  :type '(repeat :tag "Faces"
          (choice
           (const ivy-minibuffer-match-face-1)
           (const ivy-minibuffer-match-face-2)
           (const ivy-minibuffer-match-face-3)
           (const ivy-minibuffer-match-face-4)
           (face :tag "Other face"))))

(defvar ivy-flx-limit 200
  "Used to conditionally turn off flx sorting.

When the amount of matching candidates exceeds this limit, then
no sorting is done.")

(defun ivy--flx-propertize (x)
  "X is (cons (flx-score STR ...) STR)."
  (let ((str (copy-sequence (cdr x)))
        (i 0)
        (last-j -2))
    (dolist (j (cdar x))
      (unless (eq j (1+ last-j))
        (cl-incf i))
      (setq last-j j)
      (ivy-add-face-text-property
       j (1+ j)
       (nth (1+ (mod (+ i 2) (1- (length ivy-minibuffer-faces))))
            ivy-minibuffer-faces)
       str))
    str))

(defun ivy--flx-sort (name cands)
  "Sort according to closeness to string NAME the string list CANDS."
  (condition-case nil
      (let* (
             ;; an optimized regex for fuzzy matching
             ;; "abc" → "\\`[^a]*a[^b]*b[^c]*c"
             (fuzzy-regex (if (= (elt name 0) ?^)
                              (concat "^"
                                      (regexp-quote (substring name 1 2))
                                      (mapconcat
                                       (lambda (x)
                                         (setq x (string x))
                                         (concat "[^" x "]*" (regexp-quote x)))
                                       (substring name 2)
                                       ""))
                            (concat "^"
                                    (mapconcat
                                     (lambda (x)
                                       (setq x (string x))
                                       (concat "[^" x "]*" (regexp-quote x)))
                                     name
                                     ""))))

             ;; strip off the leading "^" for flx matching
             (flx-name (if (string-match "^\\^" name)
                           (substring name 1)
                         name))

             (cands-left)
             (cands-to-sort))

        ;; filter out non-matching candidates
        (dolist (cand cands)
          (when (string-match fuzzy-regex cand)
            (push cand cands-left)))

        ;; pre-sort the candidates by length before partitioning
        (setq cands-left (sort cands-left
                               (lambda (c1 c2)
                                 (< (length c1)
                                    (length c2)))))

        ;; partition the candidates into sorted and unsorted groups
        (dotimes (_n (min (length cands-left) ivy-flx-limit))
          (push (pop cands-left) cands-to-sort))

        (append
         ;; compute all of the flx scores in one pass and sort
         (mapcar #'car
                 (sort (mapcar
                        (lambda (cand)
                          (cons cand
                                (car (flx-score cand
                                                flx-name
                                                ivy--flx-cache))))
                        cands-to-sort)
                       (lambda (c1 c2)
                         ;; break ties by length
                         (if (/= (cdr c1) (cdr c2))
                             (> (cdr c1)
                                (cdr c2))
                           (< (length (car c1))
                              (length (car c2)))))))

         ;; add the unsorted candidates
         cands-left))
    (error
     cands)))

(defun ivy--truncate-string (str width)
  "Truncate STR to WIDTH."
  (if (> (string-width str) width)
      (concat (substring str 0 (min (- width 3)
                                    (- (length str) 3))) "...")
    str))

(defun ivy--format-function-generic (selected-fn other-fn cands separator)
  "Transform candidates into a string for minibuffer.
SELECTED-FN is called for the selected candidate, OTHER-FN for the others.
Both functions take one string argument each.  CANDS is a list of candidates
and SEPARATOR is used to join them."
  (let ((i -1))
    (mapconcat
     (lambda (str)
       (let ((curr (eq (cl-incf i) ivy--index)))
         (if curr
             (funcall selected-fn str)
           (funcall other-fn str))))
     cands
     separator)))

(defun ivy-format-function-default (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (ivy--add-face str 'ivy-current-match))
   #'identity
   cands
   "\n"))

(defun ivy-format-function-arrow (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "> " (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "  " str))
   cands
   "\n"))

(defun ivy-format-function-line (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (ivy--add-face (concat str "\n") 'ivy-current-match))
   (lambda (str)
     (concat str "\n"))
   cands
   ""))

(defun ivy-add-face-text-property (start end face str)
  "Add face property to the text from START to END.
FACE is the face to apply to STR."
  (if (fboundp 'add-face-text-property)
      (add-face-text-property
       start end face nil str)
    (font-lock-append-text-property
     start end 'face face str)))

(defun ivy--highlight-ignore-order (str)
  "Highlight STR, using the ignore-order method."
  (when (consp ivy--old-re)
    (let ((i 1))
      (dolist (re ivy--old-re)
        (when (string-match (car re) str)
          (ivy-add-face-text-property
           (match-beginning 0) (match-end 0)
           (nth (1+ (mod (+ i 2) (1- (length ivy-minibuffer-faces))))
                ivy-minibuffer-faces)
           str))
        (cl-incf i))))
  str)

(defun ivy--highlight-fuzzy (str)
  "Highlight STR, using the fuzzy method."
  (if ivy--flx-featurep
      (let ((flx-name (if (string-match "^\\^" ivy-text)
                          (substring ivy-text 1)
                        ivy-text)))
        (ivy--flx-propertize
         (cons (flx-score str flx-name ivy--flx-cache) str)))
    (ivy--highlight-default str)))

(defun ivy--highlight-default (str)
  "Highlight STR, using the default method."
  (unless ivy--old-re
    (setq ivy--old-re (funcall ivy--regex-function ivy-text)))
  (let ((start
         (if (and (memq (ivy-state-caller ivy-last)
                        '(counsel-git-grep counsel-ag counsel-rg counsel-pt))
                  (string-match "^[^:]+:[^:]+:" str))
             (match-end 0)
           0)))
    (ignore-errors
      (while (and (string-match ivy--old-re str start)
                  (> (- (match-end 0) (match-beginning 0)) 0))
        (setq start (match-end 0))
        (let ((i 0))
          (while (<= i ivy--subexps)
            (let ((face
                   (cond ((zerop ivy--subexps)
                          (cadr ivy-minibuffer-faces))
                         ((zerop i)
                          (car ivy-minibuffer-faces))
                         (t
                          (nth (1+ (mod (+ i 2)
                                        (1- (length ivy-minibuffer-faces))))
                               ivy-minibuffer-faces)))))
              (ivy-add-face-text-property
               (match-beginning i) (match-end i)
               face str))
            (cl-incf i))))))
  str)

(defun ivy--format-minibuffer-line (str)
  "Format line STR for use in minibuffer."
  (if (eq ivy-display-style 'fancy)
      (funcall ivy--highlight-function (copy-sequence str))
    (copy-sequence str)))

(ivy-set-display-transformer
 'counsel-find-file 'ivy-read-file-transformer)
(ivy-set-display-transformer
 'read-file-name-internal 'ivy-read-file-transformer)

(defun ivy-read-file-transformer (str)
  "Transform candidate STR when reading files."
  (if (string-match-p "/\\'" str)
      (propertize str 'face 'ivy-subdir)
    str))

(defun ivy--format (cands)
  "Return a string for CANDS suitable for display in the minibuffer.
CANDS is a list of strings."
  (setq ivy--length (length cands))
  (when (>= ivy--index ivy--length)
    (ivy-set-index (max (1- ivy--length) 0)))
  (if (null cands)
      (setf (ivy-state-current ivy-last) "")
    (let* ((half-height (/ ivy-height 2))
           (start (max 0 (- ivy--index half-height)))
           (end (min (+ start (1- ivy-height)) ivy--length))
           (start (max 0 (min start (- end (1- ivy-height)))))
           (cands (cl-subseq cands start end))
           (index (- ivy--index start))
           transformer-fn)
      (setf (ivy-state-current ivy-last) (copy-sequence (nth index cands)))
      (when (setq transformer-fn (ivy-state-display-transformer-fn ivy-last))
        (with-ivy-window
          (with-current-buffer (ivy-state-buffer ivy-last)
            (setq cands (mapcar transformer-fn cands)))))
      (let* ((ivy--index index)
             (cands (mapcar
                     #'ivy--format-minibuffer-line
                     cands))
             (res (concat "\n" (funcall ivy-format-function cands))))
        (put-text-property 0 (length res) 'read-only nil res)
        res))))

(defvar recentf-list)
(defvar bookmark-alist)

(defcustom ivy-virtual-abbreviate 'name
  "The mode of abbreviation for virtual buffer names."
  :type '(choice
          (const :tag "Only name" name)
          (const :tag "Full path" full)
          ;; eventually, uniquify
          ))
(declare-function bookmark-maybe-load-default-file "bookmark")
(declare-function bookmark-get-filename "bookmark")

(defun ivy--virtual-buffers ()
  "Adapted from `ido-add-virtual-buffers-to-list'."
  (require 'bookmark)
  (unless recentf-mode
    (recentf-mode 1))
  (let (virtual-buffers)
    (bookmark-maybe-load-default-file)
    (dolist (head (append
                   (copy-sequence recentf-list)
                   (delete "   - no file -"
                           (delq nil (mapcar #'bookmark-get-filename
                                             (copy-sequence bookmark-alist))))))
      (let ((file-name (if (stringp head)
                           head
                         (cdr head)))
            name)
        (setq name
              (if (eq ivy-virtual-abbreviate 'name)
                  (file-name-nondirectory file-name)
                (expand-file-name file-name)))
        (when (equal name "")
          (if (consp head)
              (setq name (car head))
            (setq name (file-name-nondirectory
                        (directory-file-name file-name)))))
        (and (not (equal name ""))
             (null (get-file-buffer file-name))
             (not (assoc name virtual-buffers))
             (push (cons name file-name) virtual-buffers))))
    (when virtual-buffers
      (dolist (comp virtual-buffers)
        (put-text-property 0 (length (car comp))
                           'face 'ivy-virtual
                           (car comp)))
      (setq ivy--virtual-buffers (nreverse virtual-buffers))
      (mapcar #'car ivy--virtual-buffers))))

(defcustom ivy-ignore-buffers '("\\` ")
  "List of regexps or functions matching buffer names to ignore."
  :type '(repeat (choice regexp function)))

(defvar ivy-switch-buffer-faces-alist '((dired-mode . ivy-subdir)
                                        (org-mode . org-level-4))
  "Store face customizations for `ivy-switch-buffer'.
Each KEY is `major-mode', each VALUE is a face name.")

(defun ivy--buffer-list (str &optional virtual predicate)
  "Return the buffers that match STR.
If VIRTUAL is non-nil, add virtual buffers.
If optional argument PREDICATE is non-nil, use it to test each
possible match.  See `all-completions' for further information."
  (delete-dups
   (append
    (mapcar
     (lambda (x)
       (if (with-current-buffer x
             (and default-directory
                  (file-remote-p
                   (abbreviate-file-name default-directory))))
           (propertize x 'face 'ivy-remote)
         (let ((face (with-current-buffer x
                       (cdr (assoc major-mode
                                   ivy-switch-buffer-faces-alist)))))
           (if face
               (propertize x 'face face)
             x))))
     (all-completions str 'internal-complete-buffer predicate))
    (and virtual
         (ivy--virtual-buffers)))))

(defvar ivy-views (and nil
                       `(("ivy + *scratch* {}"
                          (vert
                           (file ,(expand-file-name "ivy.el"))
                           (buffer "*scratch*")))
                         ("swiper + *scratch* {}"
                          (horz
                           (file ,(expand-file-name "swiper.el"))
                           (buffer "*scratch*")))))
  "Store window configurations selectable by `ivy-switch-buffer'.

The default value is given as an example.

Each element is a list of (NAME TREE).  NAME is a string, it's
recommended to end it with a distinctive snippet e.g. \"{}\" so
that it's easy to distinguish the window configurations.

TREE is a nested list with the following valid cars:
- vert: split the window vertically
- horz: split the window horizontally
- file: open the specified file
- buffer: open the specified buffer

TREE can be nested multiple times to have mulitple window splits.")

(defun ivy-default-view-name ()
  "Return default name for new view."
  (let* ((default-view-name
          (concat "{} "
                  (mapconcat #'identity
                             (cl-sort
                              (mapcar (lambda (w)
                                        (with-current-buffer (window-buffer w)
                                          (if (buffer-file-name)
                                              (file-name-nondirectory
                                               (buffer-file-name))
                                            (buffer-name))))
                                      (window-list))
                              #'string<)
                             " ")))
         (view-name-re (concat "\\`"
                               (regexp-quote default-view-name)
                               " \\([0-9]+\\)"))
         old-view)
    (cond ((setq old-view
                 (cl-find-if
                  (lambda (x)
                    (string-match view-name-re (car x)))
                  ivy-views))
           (format "%s %d"
                   default-view-name
                   (1+ (string-to-number
                        (match-string 1 (car old-view))))))
          ((assoc default-view-name ivy-views)
           (concat default-view-name " 1"))
          (t
           default-view-name))))

(defun ivy-push-view ()
  "Push the current window tree on `ivy-views'.
Currently, the split configuration (i.e. horizonal or vertical)
and point positions are saved, but the split positions aren't.
Use `ivy-pop-view' to delete any item from `ivy-views'."
  (interactive)
  (let* ((view (cl-labels
                   ((ft (tr)
                      (if (consp tr)
                          (if (eq (car tr) t)
                              (cons 'vert
                                    (mapcar #'ft (cddr tr)))
                            (cons 'horz
                                  (mapcar #'ft (cddr tr))))
                        (with-current-buffer (window-buffer tr)
                          (cond ((buffer-file-name)
                                 (list 'file (buffer-file-name) (point)))
                                ((eq major-mode 'dired-mode)
                                 (list 'file default-directory (point)))
                                (t
                                 (list 'buffer (buffer-name) (point))))))))
                 (ft (car (window-tree)))))
         (view-name (ivy-read "Name view: " nil
                              :initial-input (ivy-default-view-name))))
    (when view-name
      (push (list view-name view) ivy-views))))

(defun ivy-pop-view-action (view)
  "Delete VIEW from `ivy-views'."
  (setq ivy-views (delete view ivy-views))
  (setq ivy--all-candidates
        (delete (car view) ivy--all-candidates))
  (setq ivy--old-cands nil))

(defun ivy-pop-view ()
  "Delete a view to delete from `ivy-views'."
  (interactive)
  (ivy-read "Pop view: " ivy-views
            :preselect (caar ivy-views)
            :action #'ivy-pop-view-action
            :caller 'ivy-pop-view))

(defun ivy-source-views ()
  "Return the name of the views saved in `ivy-views'."
  (mapcar #'car ivy-views))

(ivy-set-sources
 'ivy-switch-buffer
 '((original-source)
   (ivy-source-views)))

(defun ivy-set-view-recur (view)
  "Set VIEW recursively."
  (cond ((eq (car view) 'vert)
         (let* ((wnd1 (selected-window))
                (wnd2 (split-window-vertically))
                (views (cdr view))
                (v (pop views)))
           (with-selected-window wnd1
             (ivy-set-view-recur v))
           (while (setq v (pop views))
             (with-selected-window wnd2
               (ivy-set-view-recur v))
             (when views
               (setq wnd2 (split-window-vertically))))))
        ((eq (car view) 'horz)
         (let* ((wnd1 (selected-window))
                (wnd2 (split-window-horizontally))
                (views (cdr view))
                (v (pop views)))
           (with-selected-window wnd1
             (ivy-set-view-recur v))
           (while (setq v (pop views))
             (with-selected-window wnd2
               (ivy-set-view-recur v))
             (when views
               (setq wnd2 (split-window-horizontally))))))
        ((eq (car view) 'file)
         (let* ((name (nth 1 view))
                (virtual (assoc name ivy--virtual-buffers))
                buffer)
           (cond ((setq buffer (get-buffer name))
                  (switch-to-buffer buffer nil 'force-same-window))
                 (virtual
                  (find-file (cdr virtual)))
                 ((file-exists-p name)
                  (find-file name))))
         (when (and (> (length view) 2)
                    (numberp (nth 2 view)))
           (goto-char (nth 2 view))))
        ((eq (car view) 'buffer)
         (switch-to-buffer (nth 1 view))
         (when (and (> (length view) 2)
                    (numberp (nth 2 view)))
           (goto-char (nth 2 view))))
        ((eq (car view) 'sexp)
         (eval (nth 1 view)))))

(defun ivy--switch-buffer-action (buffer)
  "Switch to BUFFER.
BUFFER may be a string or nil."
  (with-ivy-window
    (if (zerop (length buffer))
        (switch-to-buffer
         ivy-text nil 'force-same-window)
      (let ((virtual (assoc buffer ivy--virtual-buffers))
            (view (assoc buffer ivy-views)))
        (cond ((and virtual
                    (not (get-buffer buffer)))
               (find-file (cdr virtual)))
              (view
               (delete-other-windows)
               (let (
                     ;; silence "Directory has changed on disk"
                     (inhibit-message t))
                 (ivy-set-view-recur (cadr view))))
              (t
               (switch-to-buffer
                buffer nil 'force-same-window)))))))

(defun ivy--switch-buffer-other-window-action (buffer)
  "Switch to BUFFER in other window.
BUFFER may be a string or nil."
  (if (zerop (length buffer))
      (switch-to-buffer-other-window ivy-text)
    (let ((virtual (assoc buffer ivy--virtual-buffers)))
      (if (and virtual
               (not (get-buffer buffer)))
          (find-file-other-window (cdr virtual))
        (switch-to-buffer-other-window buffer)))))

(defun ivy--rename-buffer-action (buffer)
  "Rename BUFFER."
  (let ((new-name (read-string "Rename buffer (to new name): ")))
    (with-current-buffer buffer
      (rename-buffer new-name))))

(defvar ivy-switch-buffer-map (make-sparse-keymap))

(ivy-set-actions
 'ivy-switch-buffer
 '(("k"
    (lambda (x)
      (kill-buffer x)
      (ivy--reset-state ivy-last))
    "kill")
   ("j"
    ivy--switch-buffer-other-window-action
    "other window")
   ("r"
    ivy--rename-buffer-action
    "rename")))

(ivy-set-actions
 t
 '(("i" (lambda (x) (insert (if (stringp x) x (car x)))) "insert")
   ("w" (lambda (x) (kill-new (if (stringp x) x (car x)))) "copy")))

(defun ivy--switch-buffer-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES.
Skip buffers that match `ivy-ignore-buffers'."
  (let ((res (ivy--re-filter regexp candidates)))
    (if (or (null ivy-use-ignore)
            (null ivy-ignore-buffers))
        res
      (or (cl-remove-if
           (lambda (buf)
             (cl-find-if
              (lambda (f-or-r)
                (if (functionp f-or-r)
                    (funcall f-or-r buf)
                  (string-match-p f-or-r buf)))
              ivy-ignore-buffers))
           res)
          (and (eq ivy-use-ignore t)
               res)))))

(ivy-set-display-transformer
 'ivy-switch-buffer 'ivy-switch-buffer-transformer)
(ivy-set-display-transformer
 'internal-complete-buffer 'ivy-switch-buffer-transformer)

(defun ivy-append-face (str face)
  "Append to STR the property FACE."
  (let ((new (copy-sequence str)))
    (font-lock-append-text-property
     0 (length new) 'face face new)
    new))

(defun ivy-switch-buffer-transformer (str)
  "Transform candidate STR when switching buffers."
  (let ((b (get-buffer str)))
    (if (and b
             (buffer-file-name b)
             (buffer-modified-p b))
        (ivy-append-face str 'ivy-modified-buffer)
      str)))

(defun ivy-switch-buffer-occur ()
  "Occur function for `ivy-switch-buffer' using `ibuffer'."
  (let* ((cand-regexp
          (concat "\\(" (mapconcat #'regexp-quote ivy--old-cands "\\|") "\\)"))
         (new-qualifier `((name . ,cand-regexp))))
    (ibuffer nil (buffer-name) new-qualifier)))

;;;###autoload
(defun ivy-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (let ((this-command 'ivy-switch-buffer))
    (ivy-read "Switch to buffer: " 'internal-complete-buffer
              :matcher #'ivy--switch-buffer-matcher
              :preselect (buffer-name (other-buffer (current-buffer)))
              :action #'ivy--switch-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'ivy-switch-buffer)))

;;;###autoload
(defun ivy-switch-view ()
  "Switch to one of the window views stored by `ivy-push-view'."
  (interactive)
  (let ((ivy-initial-inputs-alist
         '((ivy-switch-buffer . "{}"))))
    (ivy-switch-buffer)))

;;;###autoload
(defun ivy-switch-buffer-other-window ()
  "Switch to another buffer in another window."
  (interactive)
  (ivy-read "Switch to buffer in other window: " 'internal-complete-buffer
            :matcher #'ivy--switch-buffer-matcher
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-other-window-action
            :keymap ivy-switch-buffer-map
            :caller 'ivy-switch-buffer-other-window))

(define-obsolete-function-alias 'ivy-recentf 'counsel-recentf "0.8.0")

(defun ivy-yank-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (let (amend)
    (with-ivy-window
      (let ((pt (point))
            (le (line-end-position)))
        (forward-word 1)
        (if (> (point) le)
            (goto-char pt)
          (setq amend (buffer-substring-no-properties pt (point))))))
    (when amend
      (insert (replace-regexp-in-string "  +" " " amend)))))

(defun ivy-kill-ring-save ()
  "Store the current candidates into the kill ring.
If the region is active, forward to `kill-ring-save' instead."
  (interactive)
  (if (region-active-p)
      (call-interactively 'kill-ring-save)
    (kill-new
     (mapconcat
      #'identity
      ivy--old-cands
      "\n"))))

(defun ivy-insert-current ()
  "Make the current candidate into current input.
Don't finish completion."
  (interactive)
  (delete-minibuffer-contents)
  (if (and ivy--directory
           (string-match "/$" (ivy-state-current ivy-last)))
      (insert (substring (ivy-state-current ivy-last) 0 -1))
    (insert (ivy-state-current ivy-last))))

(defcustom ivy--preferred-re-builders
  '((ivy--regex-plus . "ivy")
    (ivy--regex-ignore-order . "order")
    (ivy--regex-fuzzy . "fuzzy"))
  "Alist of preferred re-builders with display names.
This list can be rotated with `ivy-rotate-preferred-builders'."
  :type '(alist :key-type function :value-type string)
  :group 'ivy)

(defun ivy-rotate-preferred-builders ()
  "Switch to the next re builder in `ivy--preferred-re-builders'."
  (interactive)
  (when ivy--preferred-re-builders
    (setq ivy--old-re nil)
    (setq ivy--regex-function
          (let ((cell (assoc ivy--regex-function ivy--preferred-re-builders)))
            (car (or (cadr (memq cell ivy--preferred-re-builders))
                     (car ivy--preferred-re-builders)))))))

(defun ivy-toggle-fuzzy ()
  "Toggle the re builder between `ivy--regex-fuzzy' and `ivy--regex-plus'."
  (interactive)
  (setq ivy--old-re nil)
  (if (eq ivy--regex-function 'ivy--regex-fuzzy)
      (setq ivy--regex-function 'ivy--regex-plus)
    (setq ivy--regex-function 'ivy--regex-fuzzy)))

(defun ivy-reverse-i-search ()
  "Enter a recursive `ivy-read' session using the current history.
The selected history element will be inserted into the minibuffer."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (history (symbol-value (ivy-state-history ivy-last)))
        (old-last ivy-last)
        (ivy-recursive-restore nil))
    (ivy-read "Reverse-i-search: "
              history
              :action (lambda (x)
                        (ivy--reset-state
                         (setq ivy-last old-last))
                        (delete-minibuffer-contents)
                        (insert (substring-no-properties x))
                        (ivy--cd-maybe)))))

(defun ivy-restrict-to-matches ()
  "Restrict candidates to current input and erase input."
  (interactive)
  (delete-minibuffer-contents)
  (setq ivy--all-candidates
        (ivy--filter ivy-text ivy--all-candidates)))

;;* Occur
(defvar-local ivy-occur-last nil
  "Buffer-local value of `ivy-last'.
Can't re-use `ivy-last' because using e.g. `swiper' in the same
buffer would modify `ivy-last'.")

(defvar ivy-occur-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'ivy-occur-click)
    (define-key map (kbd "RET") 'ivy-occur-press-and-switch)
    (define-key map (kbd "j") 'ivy-occur-next-line)
    (define-key map (kbd "k") 'ivy-occur-previous-line)
    (define-key map (kbd "h") 'backward-char)
    (define-key map (kbd "l") 'forward-char)
    (define-key map (kbd "f") 'ivy-occur-press)
    (define-key map (kbd "g") 'ivy-occur-revert-buffer)
    (define-key map (kbd "a") 'ivy-occur-read-action)
    (define-key map (kbd "o") 'ivy-occur-dispatch)
    (define-key map (kbd "c") 'ivy-occur-toggle-calling)
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for Ivy Occur mode.")

(defun ivy-occur-toggle-calling ()
  "Toggle `ivy-calling'."
  (interactive)
  (if (setq ivy-calling (not ivy-calling))
      (progn
        (setq mode-name "Ivy-Occur [calling]")
        (ivy-occur-press))
    (setq mode-name "Ivy-Occur"))
  (force-mode-line-update))

(defun ivy-occur-next-line (&optional arg)
  "Move the cursor down ARG lines.
When `ivy-calling' isn't nil, call `ivy-occur-press'."
  (interactive "p")
  (forward-line arg)
  (when ivy-calling
    (ivy-occur-press)))

(defun ivy-occur-previous-line (&optional arg)
  "Move the cursor up ARG lines.
When `ivy-calling' isn't nil, call `ivy-occur-press'."
  (interactive "p")
  (forward-line (- arg))
  (when ivy-calling
    (ivy-occur-press)))

(define-derived-mode ivy-occur-mode fundamental-mode "Ivy-Occur"
  "Major mode for output from \\[ivy-occur].

\\{ivy-occur-mode-map}"
  (setq-local view-read-only nil))

(defvar ivy-occur-grep-mode-map
  (let ((map (copy-keymap ivy-occur-mode-map)))
    (define-key map (kbd "C-x C-q") 'ivy-wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-d") 'ivy-occur-delete-candidate)
    map)
  "Keymap for Ivy Occur Grep mode.")

(defun ivy-occur-delete-candidate ()
  (interactive)
  (let ((inhibit-read-only t))
    (delete-region (line-beginning-position)
                   (1+ (line-end-position)))))

(define-derived-mode ivy-occur-grep-mode grep-mode "Ivy-Occur"
  "Major mode for output from \\[ivy-occur].

\\{ivy-occur-grep-mode-map}"
  (setq-local view-read-only nil)
  (when (fboundp 'wgrep-setup)
    (wgrep-setup)))

(defvar ivy--occurs-list nil
  "A list of custom occur generators per command.")

(defun ivy-set-occur (cmd occur)
  "Assign CMD a custom OCCUR function."
  (setq ivy--occurs-list
        (plist-put ivy--occurs-list cmd occur)))

(ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
(ivy-set-occur 'ivy-switch-buffer-other-window 'ivy-switch-buffer-occur)

(defun ivy--occur-insert-lines (cands)
  "Insert CANDS into `ivy-occur' buffer."
  (dolist (str cands)
    (add-text-properties
     0 (length str)
     `(mouse-face
       highlight
       help-echo "mouse-1: call ivy-action")
     str)
    (insert str "\n"))
  (goto-char (point-min))
  (forward-line 4))

(defun ivy-occur ()
  "Stop completion and put the current candidates into a new buffer.

The new buffer remembers current action(s).

While in the *ivy-occur* buffer, selecting a candidate with RET or
a mouse click will call the appropriate action for that candidate.

There is no limit on the number of *ivy-occur* buffers."
  (interactive)
  (if (not (window-minibuffer-p))
      (user-error "No completion session is active")
    (let* ((caller (ivy-state-caller ivy-last))
           (occur-fn (plist-get ivy--occurs-list caller))
           (buffer
            (generate-new-buffer
             (format "*ivy-occur%s \"%s\"*"
                     (if caller
                         (concat " " (prin1-to-string caller))
                       "")
                     ivy-text))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (if occur-fn
              (funcall occur-fn)
            (ivy-occur-mode)
            (insert (format "%d candidates:\n" (length ivy--old-cands)))
            (read-only-mode)
            (ivy--occur-insert-lines
             (mapcar
              (lambda (cand) (concat "    " cand))
              ivy--old-cands))))
        (setf (ivy-state-text ivy-last) ivy-text)
        (setq ivy-occur-last ivy-last)
        (setq-local ivy--directory ivy--directory))
      (ivy-exit-with-action
       `(lambda (_) (pop-to-buffer ,buffer))))))

(defun ivy-occur-revert-buffer ()
  "Refresh the buffer making it up-to date with the collection.

Currently only works for `swiper'.  In that specific case, the
*ivy-occur* buffer becomes nearly useless as the orignal buffer
is updated, since the line numbers no longer match.

Calling this function is as if you called `ivy-occur' on the
updated original buffer."
  (interactive)
  (let ((caller (ivy-state-caller ivy-occur-last))
        (ivy-last ivy-occur-last))
    (cond ((eq caller 'swiper)
           (let ((buffer (ivy-state-buffer ivy-occur-last)))
             (unless (buffer-live-p buffer)
               (error "Buffer was killed"))
             (let ((inhibit-read-only t))
               (erase-buffer)
               (funcall (plist-get ivy--occurs-list caller) t)
               (ivy-occur-grep-mode))))
          ((memq caller '(counsel-git-grep counsel-grep counsel-ag counsel-rg))
           (let ((inhibit-read-only t))
             (erase-buffer)
             (funcall (plist-get ivy--occurs-list caller)))))
    (setq ivy-occur-last ivy-last)))

(declare-function wgrep-change-to-wgrep-mode "ext:wgrep")

(defun ivy-wgrep-change-to-wgrep-mode ()
  "Forward to `wgrep-change-to-wgrep-mode'."
  (interactive)
  (if (require 'wgrep nil 'noerror)
      (wgrep-change-to-wgrep-mode)
    (error "Package wgrep isn't installed")))

(defun ivy-occur-read-action ()
  "Select one of the available actions as the current one."
  (interactive)
  (let ((ivy-last ivy-occur-last))
    (ivy-read-action)))

(defun ivy-occur-dispatch ()
  "Call one of the available actions on the current item."
  (interactive)
  (let* ((state-action (ivy-state-action ivy-occur-last))
         (actions (if (symbolp state-action)
                      state-action
                    (copy-sequence state-action))))
    (unwind-protect
         (progn
           (ivy-occur-read-action)
           (ivy-occur-press))
      (setf (ivy-state-action ivy-occur-last) actions))))

(defun ivy-occur-click (event)
  "Execute action for the current candidate.
EVENT gives the mouse position."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (with-current-buffer (window-buffer window)
      (goto-char pos)
      (ivy-occur-press))))

(declare-function swiper--cleanup "swiper")
(declare-function swiper--add-overlays "swiper")
(defvar ivy-occur-timer nil)
(defvar counsel-grep-last-line)

(defun ivy-occur-press ()
  "Execute action for the current candidate."
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at "\\(?:./\\|    \\)\\(.*\\)$"))
    (when (memq (ivy-state-caller ivy-occur-last)
                '(swiper counsel-git-grep counsel-grep counsel-ag counsel-rg
                  counsel-describe-function counsel-describe-variable))
      (let ((window (ivy-state-window ivy-occur-last)))
        (when (or (null (window-live-p window))
                  (equal window (selected-window)))
          (save-selected-window
            (setf (ivy-state-window ivy-occur-last)
                  (display-buffer (ivy-state-buffer ivy-occur-last)
                                  'display-buffer-pop-up-window))))))
    (let* ((ivy-last ivy-occur-last)
           (ivy-text (ivy-state-text ivy-last))
           (str (buffer-substring
                 (match-beginning 1)
                 (match-end 1)))
           (coll (ivy-state-collection ivy-last))
           (action (ivy--get-action ivy-last))
           (ivy-exit 'done))
      (with-ivy-window
        (setq counsel-grep-last-line nil)
        (with-current-buffer (ivy-state-buffer ivy-last)
          (funcall action
                   (if (and (consp coll)
                            (consp (car coll)))
                       (assoc str coll)
                     str)))
        (if (memq (ivy-state-caller ivy-last)
                  '(swiper counsel-git-grep counsel-grep counsel-ag counsel-rg))
            (with-current-buffer (window-buffer (selected-window))
              (swiper--cleanup)
              (swiper--add-overlays
               (ivy--regex ivy-text)
               (line-beginning-position)
               (line-end-position)
               (selected-window))
              (when (timerp ivy-occur-timer)
                (cancel-timer ivy-occur-timer))
              (setq ivy-occur-timer
                    (run-at-time 1.0 nil 'swiper--cleanup))))))))

(defun ivy-occur-press-and-switch ()
  "Execute action for the current candidate and switch window."
  (interactive)
  (ivy-occur-press)
  (select-window (ivy--get-window ivy-occur-last)))

(defconst ivy-help-file (let ((default-directory
                               (if load-file-name
                                   (file-name-directory load-file-name)
                                 default-directory)))
                          (if (file-exists-p "ivy-help.org")
                              (expand-file-name "ivy-help.org")
                            (if (file-exists-p "doc/ivy-help.org")
                                (expand-file-name "doc/ivy-help.org"))))
  "The file for `ivy-help'.")

(defun ivy-help ()
  "Help for `ivy'."
  (interactive)
  (let ((buf (get-buffer "*Ivy Help*")))
    (unless buf
      (setq buf (get-buffer-create "*Ivy Help*"))
      (with-current-buffer buf
        (insert-file-contents ivy-help-file)
        (org-mode)
        (view-mode)
        (goto-char (point-min))))
    (if (eq this-command 'ivy-help)
        (switch-to-buffer buf)
      (with-ivy-window
        (pop-to-buffer buf)))
    (view-mode)
    (goto-char (point-min))))

(provide 'ivy)

;;; ivy.el ends here
