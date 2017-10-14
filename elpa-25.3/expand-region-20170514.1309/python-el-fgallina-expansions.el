;;; python-el-fgallina-expansions.el --- fgallina/python.el-specific expansions for expand-region

;; Copyright (C) 2012 Felix Geller

;; Author: Felix Geller
;; Keywords: marking region python

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
;;  - Additions implemented here:
;;    - `er/mark-inside-python-string'
;;    - `er/mark-outside-python-string'
;;    - `er/mark-python-statement'
;;    - `er/mark-python-block'
;;    - `er/mark-outer-python-block'
;;    - `er/mark-python-block-and-decorator'
;;  - Supports multi-line strings

;;; Code:

(require 'expand-region-core)

(if (not (fboundp 'python-syntax-context))
    (defalias 'python-syntax-context 'python-info-ppss-context))
(if (not (fboundp 'python-indent-offset))
    (defalias 'python-indent-offset 'python-indent))

(defvar er--python-string-delimiter
  "'\""
  "Characters that delimit a Python string.")

;; copied from @fgallina's python.el as a quick fix. The variable
;; `python-rx-constituents' is not bound when we use the python-rx
;; macro from here, so we have to construct the regular expression
;; manually.
(defvar er--python-block-start-regex
  (rx symbol-start
      (or "def" "class" "if" "elif" "else" "try"
          "except" "finally" "for" "while" "with")
      symbol-end)
  "Regular expression string to match the beginning of a Python block.")

(defun er/mark-python-string (mark-inside)
  "Mark the Python string that surrounds point.

If the optional MARK-INSIDE is not nil, only mark the region
between the string delimiters, otherwise the region includes the
delimiters as well."
  (let ((beginning-of-string (python-syntax-context 'string (syntax-ppss))))
    (when beginning-of-string
      (goto-char beginning-of-string)
      ;; Move inside the string, so we can use ppss to find the end of
      ;; the string.
      (skip-chars-forward er--python-string-delimiter)
      (while (python-syntax-context 'string (syntax-ppss))
        (forward-char 1))
      (when mark-inside (skip-chars-backward er--python-string-delimiter))
      (set-mark (point))
      (goto-char beginning-of-string)
      (when mark-inside (skip-chars-forward er--python-string-delimiter)))))

(defun er/mark-inside-python-string ()
  "Mark the inside of the Python string that surrounds point.

Command that wraps `er/mark-python-string'."
  (interactive)
  (er/mark-python-string t))

(defun er/mark-outside-python-string ()
  "Mark the outside of the Python string that surrounds point.

Command that wraps `er/mark-python-string'."
  (interactive)
  (er/mark-python-string nil))

(defun er/mark-python-statement ()
  "Mark the Python statement that surrounds point."
  (interactive)
  (python-nav-end-of-statement)
  (set-mark (point))
  (python-nav-beginning-of-statement))

(defun er/mark-python-block (&optional next-indent-level)
  "Mark the Python block that surrounds point.

If the optional NEXT-INDENT-LEVEL is given, select the
surrounding block that is defined at an indentation that is less
than NEXT-INDENT-LEVEL."
  (interactive)
  (back-to-indentation)
  (let ((next-indent-level
         (or
          ;; Use the given level
          next-indent-level
          ;; Check whether point is at the start of a Python block.
          (if (looking-at er--python-block-start-regex)
              ;; Block start means that the next level is deeper.
              (+ (current-indentation) python-indent-offset)
            ;; Assuming we're inside the block that we want to mark
            (current-indentation)))))
    ;; Move point to next Python block start at the correct indent-level
    (while (>= (current-indentation) next-indent-level)
      (re-search-backward er--python-block-start-regex))
    ;; Mark the beginning of the block
    (set-mark (point))
    ;; Save indentation and look for the end of this block
    (let ((block-indentation (current-indentation)))
      (forward-line 1)
      (while (and
              ;; No need to go beyond the end of the buffer. Can't use
              ;; eobp as the loop places the point at the beginning of
              ;; line, but eob might be at the end of the line.
              (not (= (point-max) (point-at-eol)))
              ;; Proceed if: indentation is too deep
              (or (> (current-indentation) block-indentation)
                  ;; Looking at an empty line
                  (looking-at (rx line-start (* whitespace) line-end))
                  ;; We're not looking at the start of a Python block
                  ;; and the indent is deeper than the block's indent
                  (and (not (looking-at er--python-block-start-regex))
                       (> (current-indentation) block-indentation))))
        (forward-line 1)
        (back-to-indentation))
      ;; Find the end of the block by skipping comments backwards
      (python-util-forward-comment -1)
      (exchange-point-and-mark))))

(defun er/mark-outer-python-block ()
  "Mark the Python block that surrounds the Python block around point.

Command that wraps `er/mark-python-block'."
  (interactive)
  (er/mark-python-block (current-indentation)))

(defun er/mark-python-block-and-decorator ()
  (interactive)
  (back-to-indentation)
  (if (or (er--python-looking-at-decorator) (er--python-looking-at-decorator -1))
      (progn
	(while (er--python-looking-at-decorator -1)
	  (forward-line -1)
	  (back-to-indentation)
	  )
	(set-mark (point))
	(while (er--python-looking-at-decorator)
	  (forward-line)
	  )
	(python-nav-end-of-block)
	(exchange-point-and-mark))))

(defun er--python-looking-at-decorator (&optional line-offset)
  (save-excursion
    (if line-offset
	(forward-line line-offset)
	)
    (back-to-indentation)
    (looking-at "@")
    ))

(defun er/add-python-mode-expansions ()
  "Adds python-mode-specific expansions for buffers in python-mode"
  (let ((try-expand-list-additions '(
                                     er/mark-inside-python-string
                                     er/mark-outside-python-string
                                     er/mark-python-statement
                                     er/mark-python-block
				     er/mark-python-block-and-decorator
                                     er/mark-outer-python-block
                                     )))
    (set (make-local-variable 'expand-region-skip-whitespace) nil)
    (set (make-local-variable 'er/try-expand-list)
         (remove 'er/mark-inside-quotes
                 (remove 'er/mark-outside-quotes
                         (append er/try-expand-list try-expand-list-additions))))))

(er/enable-mode-expansions 'python-mode 'er/add-python-mode-expansions)

(provide 'python-el-fgallina-expansions)

;; python-el-fgallina-expansions.el ends here
