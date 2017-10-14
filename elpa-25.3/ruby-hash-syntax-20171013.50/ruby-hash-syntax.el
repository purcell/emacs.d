;;; ruby-hash-syntax.el --- Toggle ruby hash syntax between classic and 1.9 styles

;; Copyright (C) 2013-2017  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Package-Version: 20171013.50
;; Package-X-Original-Version: 0
;; URL: https://github.com/purcell/ruby-hash-syntax
;; Keywords: languages

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

;; Adapted from the method used by TextMate, this library provides
;; a command `ruby-hash-syntax-toggle' which attempts to automatically
;; convert the selected region of ruby code between 1.8 and 1.9 hash styles.

;;; Code:

;; Borrowed from https://github.com/textmate/ruby.tmbundle/blob/master/Commands/Convert%20Ruby%20hash%20to%201_9%20syntax.tmCommand

;;;###autoload
(defun ruby-hash-syntax-toggle (beg end)
  "Toggle syntax of ruby hash literal in region from BEG to END between ruby 1.8 and 1.9 styles."
  (interactive "r")
  (unless (use-region-p)
    (error "The region is not active"))
  (save-excursion
    (let ((limit (copy-marker (max beg end)))
          (hashrocket-pattern "[^:]:\\([a-zA-Z0-9_]+\\) *=> *"))
      (goto-char (min beg end))
      (cond
       ((ruby-hash-syntax--code-has-pattern hashrocket-pattern limit)
        (ruby-hash-syntax--replace hashrocket-pattern "\\1: " limit))
       ((ruby-hash-syntax--code-has-pattern "\\w+:" limit)
        (ruby-hash-syntax--replace "\\([a-zA-Z0-9_]+\\):\\( *\\(?:\"\\(?:\\\"\\|[^\"]\\)*\"\\|'\\(?:\\'\\|[^']\\)*'\\|[a-zA-Z0-9_]+([^)]*)\\|[^,]+\\)\\)" ":\\1 =>\\2" limit))))))

;;;###autoload
(define-obsolete-function-alias 'ruby-toggle-hash-syntax 'ruby-hash-syntax-toggle)

(defun ruby-hash-syntax--code-has-pattern (pat limit)
  "A version of `search-forward' which skips over string literals.
Argument PAT is the search patter, while LIMIT is the maximum
search extent."
  (catch 'found
    (save-excursion
      (while (re-search-forward pat limit t)
        (unless (elt (syntax-ppss) 3)
          ;; If this isn't inside a string...
          (throw 'found t))))))

(defun ruby-hash-syntax--replace (from to end)
  "Replace FROM with TO up to END."
  (while (re-search-forward from end t)
    (replace-match to nil nil)))


(provide 'ruby-hash-syntax)
;;; ruby-hash-syntax.el ends here
