;;; cc-mode-expansions.el --- C-specific expansions for expand-region

;; Copyright (C) 2012 François Févotte

;; Author: François Févotte
;; Based on js-mode-expansions by: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking region

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Extra expansions for C-like modes that I've found useful so far:
;;
;; er/c-mark-statement
;;     Captures simple and more complex statements.
;;
;; er/c-mark-fully-qualified-name
;;     Captures identifiers composed of several '::'-separated parts.
;;
;; er/c-mark-function-call[-1|-2]
;;     Captures an identifier followed by a '()'-enclosed block.
;;
;; er/c-mark-statement-block[-1|-2]
;;     Captures a statement followed by a '{}'-enclosed block.
;;     This matches function definitions and if/for/... constructs.
;;
;; er/c-mark-vector-access[-1|-2]
;;     Captures an identifier followed by a '[]'-enclosed block.
;;
;; Feel free to contribute any other expansions for C at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(require 'expand-region-core)
(require 'er-basic-expansions)
(require 'cc-cmds)

(defun er/c-mark-statement ()
  "Mark the current C statement.

This function tries to ensure that pair-delimited substring are
either fully inside or fully outside the statement."
  (interactive)
  (unless (use-region-p)
    (set-mark (point)))

  (if (< (point) (mark))
      (exchange-point-and-mark))

  ;; Contract the region a bit to make the
  ;; er/c-mark-statement function idempotent
  (when (>= (- (point) (mark)) 2)
    (exchange-point-and-mark)
    (forward-char)
    (exchange-point-and-mark)
    (backward-char))

  (let (beg end)
    ;; Determine boundaries of the outside-pairs region
    (save-mark-and-excursion
      (c-end-of-statement)
      (er/mark-outside-pairs)
      (setq beg (point)
            end (mark)))

    ;; Determine boundaries of the statement as given
    ;; by c-beginning-of-statement/c-end-of-statement
    (c-end-of-statement)
    (exchange-point-and-mark)
    (c-end-of-statement)(c-beginning-of-statement 1)

    ;; If the two regions overlap, expand the region
    (cond ((and (<= (point) beg)
                (<  (mark)  end))
           (set-mark end))
          ((and (>  (point) beg)
                (>= (mark)  end))
           (goto-char beg)
           (c-end-of-statement)
           (c-beginning-of-statement 1)))))

(defun er/c-mark-fully-qualified-name ()
  "Mark the current C++ fully qualified identifier.

This function captures identifiers composed of multiple
'::'-separated parts."
  (interactive)
  (er/mark-symbol)
  (when (use-region-p)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (while (er/looking-back-exact "::")
      (backward-char 2)
      (skip-syntax-backward "_w"))
    (exchange-point-and-mark)
    (while (looking-at "::")
      (forward-char 2)
      (skip-syntax-forward "_w"))
    (exchange-point-and-mark)))

(defmacro er/c-define-construct (name mark-first-part open-brace doc)
  (let ((docstring (make-symbol "docstring-tmp")))
    (setq docstring
          (concat
           doc "\n\n"
           "This function tries to mark a region consisting of two parts:\n"
           (format " - the first part is marked using `%s'\n" (symbol-name mark-first-part))
           (format " - the second part is a block beginning with '%s'\n\n" open-brace)))
    `(progn
       (defun ,(intern (concat (symbol-name name) "-1")) ()
         ,(concat docstring
                  "This function assumes that point is in the first part and the\n"
                  "region is active.\n\n"
                  (format "See also `%s'." (concat (symbol-name name) "-2")))
         (interactive)
         (when (use-region-p)
           (,mark-first-part)
           (exchange-point-and-mark)
           (let ((oldpos (point)))
             (skip-syntax-forward " ")
             (if (looking-at ,open-brace)
                 (progn (forward-sexp)
                        (exchange-point-and-mark))
               (goto-char oldpos)))))
       (defun ,(intern (concat (symbol-name name) "-2")) ()
         ,(concat docstring
                  "This function assumes that the block constituting the second part\n"
                  "is already marked and active.\n\n"
                  (format "See also `%s'." (concat (symbol-name name) "-1")))
         (interactive)
         (when (use-region-p)
           (when (> (point) (mark))
             (exchange-point-and-mark))
           (when (looking-at ,open-brace)
             (let ((beg (point))
                   (end (progn (forward-sexp 1)
                               (point))))
               (goto-char beg)
               (skip-syntax-backward " ")
               (backward-char)
               (deactivate-mark)
               (,mark-first-part)
               (set-mark end))))))))

(er/c-define-construct er/c-mark-function-call er/c-mark-fully-qualified-name "("
                       "Mark the current function call.")
(er/c-define-construct er/c-mark-statement-block er/c-mark-statement "{"
                       "Mark the current block construct (like if, for, etc.)")
(er/c-define-construct er/c-mark-vector-access er/c-mark-fully-qualified-name "\\["
                       "Mark the current vector access.")

(defun er/add-cc-mode-expansions ()
  "Adds expansions for buffers in c-mode."
  (set (make-local-variable 'er/try-expand-list)
       (append er/try-expand-list
               '(er/c-mark-statement
                 er/c-mark-fully-qualified-name
                 er/c-mark-function-call-1   er/c-mark-function-call-2
                 er/c-mark-statement-block-1 er/c-mark-statement-block-2
                 er/c-mark-vector-access-1   er/c-mark-vector-access-2))))

(er/enable-mode-expansions 'c-mode 'er/add-cc-mode-expansions)
(er/enable-mode-expansions 'c++-mode 'er/add-cc-mode-expansions)
(er/enable-mode-expansions 'objc-mode 'er/add-cc-mode-expansions)
(er/enable-mode-expansions 'java-mode 'er/add-cc-mode-expansions)
(er/enable-mode-expansions 'idl-mode 'er/add-cc-mode-expansions)
(er/enable-mode-expansions 'pike-mode 'er/add-cc-mode-expansions)
(er/enable-mode-expansions 'awk-mode 'er/add-cc-mode-expansions)

(provide 'cc-mode-expansions)

;; cc-mode-expansions.el ends here
