;;; haskell-utils.el --- General utility functions used by haskell-mode modules -*- lexical-binding: t -*-

;; Copyright © 2013 Herbert Valerio Riedel
;;             2016 Arthur Fayzrakhmanov

;; Author: Herbert Valerio Riedel <hvr@gnu.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module's purpose is to provide a place for helper functions
;; which are general enough to be usable by multiple modules and/or
;; to alleviate circular module dependency problems.
;;
;; When possible, functions in this module shall be accompanied by
;; ERT-based unit tests.
;;
;; See also `haskell-str.el' for string utility functions.
;;
;; All symbols in this module have a `haskell-utils-' prefix.

;;; Code:

;; =============================================================================
;;                                     NOTE:
;; THIS MODULE IS SUPPOSED TO BE A LEAF-MODULE AND SHALL NOT REQUIRE/DEPEND-ON
;; ANY OTHER HASKELL-MODE MODULES IN ORDER TO STAY AT THE BOTTOM OF THE MODULE
;; DEPENDENCY GRAPH.
;; =============================================================================

(eval-when-compile (require 'cl-lib))

(defvar-local haskell-utils-async-post-command-flag nil
  "Non-nil means some commands were triggered during async function execution.")

(defvar haskell-mode-interactive-prompt-state nil
  "Special variable indicating a state of user input waiting.")

(defun haskell-utils-read-directory-name (prompt default)
  "Read directory name and normalize to true absolute path.
Refer to `read-directory-name' for the meaning of PROMPT and
DEFAULT.  If `haskell-process-load-or-reload-prompt' is nil,
accept `default'."
  (let ((filename (file-truename (read-directory-name prompt default default))))
    (concat (replace-regexp-in-string "/$" "" filename) "/")))

(defun haskell-utils-parse-import-statement-at-point ()
  "Return imported module name if on import statement or nil otherwise.
This currently assumes that the \"import\" keyword and the module
name are on the same line.

This function supports the SafeHaskell and PackageImports syntax extensions.

Note: doesn't detect if in {--}-style comment."
  (save-excursion
    (goto-char (line-beginning-position))
    (if (looking-at (concat "[\t ]*import[\t ]+"
                            "\\(?:safe[\t ]+\\)?" ;; SafeHaskell
                            "\\(?:qualified[\t ]+\\)?"
                            "\\(?:\"[^\"]*\"[\t ]+\\)?" ;; PackageImports
                            "\\([[:digit:][:upper:][:lower:]_.]+\\)"))
        (match-string-no-properties 1))))

(defun haskell-utils-async-update-post-command-flag ()
  "A special hook which collects triggered commands during async execution.
This hook pushes value of variable `this-command' to flag variable
`haskell-utils-async-post-command-flag'."
  (let* ((cmd this-command)
         (updated-flag (cons cmd haskell-utils-async-post-command-flag)))
    (setq haskell-utils-async-post-command-flag updated-flag)))

(defun haskell-utils-async-watch-changes ()
  "Watch for triggered commands during async operation execution.
Resets flag variable
`haskell-utils-async-update-post-command-flag' to NIL.  By changes it is
assumed that nothing happened, e.g. nothing was inserted in
buffer, point was not moved, etc.  To collect data `post-command-hook' is used."
  (setq haskell-utils-async-post-command-flag nil)
  (add-hook
   'post-command-hook #'haskell-utils-async-update-post-command-flag nil t))

(defun haskell-utils-async-stop-watching-changes (buffer)
  "Clean up after async operation finished.
This function takes care about cleaning up things made by
`haskell-utils-async-watch-changes'.  The BUFFER argument is a buffer where
`post-command-hook' should be disabled.  This is neccessary, because
it is possible that user will change buffer during async function
execusion."
  (with-current-buffer buffer
    (setq haskell-utils-async-post-command-flag nil)
    (remove-hook
     'post-command-hook #'haskell-utils-async-update-post-command-flag t)))

(defun haskell-utils-reduce-string (str)
  "Remove newlines and extra whitespace from string STR.
If line starts with a sequence of whitespaces, substitutes this
sequence with a single whitespace.  Removes all newline
characters."
  (let ((s (replace-regexp-in-string "^\s+" " " str)))
    (replace-regexp-in-string "\r?\n" "" s)))

(defun haskell-utils-repl-response-error-status (response)
  "Parse response REPL's RESPONSE for errors.
Returns one of the following symbols:

+ unknown-command
+ option-missing
+ interactive-error
+ no-error

*Warning*: this funciton covers only three kind of responses:

* \"unknown command …\"
  REPL missing requested command
* \"<interactive>:3:5: …\"
  interactive REPL error
* \"Couldn't guess that module name. Does it exist?\"
  (:type-at and maybe some other commands error)
* *all other reposnses* are treated as success reposneses and
  'no-error is returned."
  (if response
      (let ((first-line (car (split-string response "\n" t))))
        (cond
         ((null first-line) 'no-error)
         ((string-match-p "^unknown command" first-line)
          'unknown-command)
         ((string-match-p
           "^Couldn't guess that module name. Does it exist?"
           first-line)
          'option-missing)
         ((string-match-p "^<interactive>:" first-line)
          'interactive-error)
         (t 'no-error)))
    ;; in case of nil-ish reponse it's not clear is it error response or not
    'no-error))

(defun haskell-utils-compose-type-at-command (pos)
  "Prepare :type-at command to be send to haskell process.
POS is a cons cell containing min and max positions, i.e. target
expression bounds."
  (save-excursion
    (let ((start-p (car pos))
          (end-p (cdr pos))
          start-l
          start-c
          end-l
          end-c
          value)
      (goto-char start-p)
      (setq start-l (line-number-at-pos))
      (setq start-c (1+ (current-column)))
      (goto-char end-p)
      (setq end-l (line-number-at-pos))
      (setq end-c (1+ (current-column)))
      (setq value (buffer-substring-no-properties start-p end-p))
      ;; supress multiline expressions
      (let ((lines (split-string value "\n" t)))
        (when (and (cdr lines)
                   (stringp (car lines)))
          (setq value (format "[ %s … ]" (car lines)))))
      (replace-regexp-in-string
       "\n$"
       ""
       (format ":type-at %s %d %d %d %d %s"
               (buffer-file-name)
               start-l
               start-c
               end-l
               end-c
               value)))))


(defun haskell-mode-toggle-interactive-prompt-state (&optional disabled)
  "Set `haskell-mode-interactive-prompt-state' to t.
If given DISABLED argument sets variable value to nil, otherwise to t."
  (setq haskell-mode-interactive-prompt-state (not disabled)))

(provide 'haskell-utils)
;;; haskell-utils.el ends here
