;;; company-dabbrev-code.el --- dabbrev-like company-mode back-end for code

;; Copyright (C) 2009, 2011  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'company-dabbrev)
(eval-when-compile (require 'cl))

(defgroup company-dabbrev-code nil
  "dabbrev-like completion back-end for code."
  :group 'company)

(defcustom company-dabbrev-code-modes
  '(asm-mode batch-file-mode c++-mode c-mode cperl-mode csharp-mode css-mode
    emacs-lisp-mode erlang-mode f90-mode fortran-mode haskell-mode java-mode
    javascript-mode jde-mode js2-mode lisp-mode lua-mode objc-mode perl-mode
    php-mode prog-mode python-mode ruby-mode scheme-mode shell-script-mode)
  "Modes that use `company-dabbrev-code'.
In all these modes `company-dabbrev-code' will complete only symbols, not text
in comments or strings.  In other modes `company-dabbrev-code' will pass control
to other back-ends \(e.g. `company-dabbrev'\).
Value t means complete in all modes."
  :type '(choice (repeat (symbol :tag "Major mode"))
                 (const tag "All modes" t)))

(defcustom company-dabbrev-code-other-buffers t
  "Determines whether `company-dabbrev-code' should search other buffers.
If `all', search all other buffers.  If t, search buffers with the same
major mode.
See also `company-dabbrev-code-time-limit'."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Same major mode" t)
                 (const :tag "All" all)))

(defcustom company-dabbrev-code-time-limit .1
  "Determines how long `company-dabbrev-code' should look for matches."
  :type '(choice (const :tag "Off" nil)
                 (number :tag "Seconds")))

(defcustom company-dabbrev-code-everywhere nil
  "Non-nil to offer completions in comments and strings."
  :type 'boolean)

(defcustom company-dabbrev-code-ignore-case nil
  "Non-nil to ignore case in completion candidates."
  :type 'boolean)

(defsubst company-dabbrev-code--make-regexp (prefix)
  (concat "\\_<" (if (equal prefix "")
                     "\\([a-zA-Z]\\|\\s_\\)"
                   (regexp-quote prefix))
          "\\(\\sw\\|\\s_\\)*\\_>"))

;;;###autoload
(defun company-dabbrev-code (command &optional arg &rest ignored)
  "dabbrev-like `company-mode' back-end for code.
The back-end looks for all symbols in the current buffer that aren't in
comments or strings."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-dabbrev-code))
    (prefix (and (or (eq t company-dabbrev-code-modes)
                     (apply 'derived-mode-p company-dabbrev-code-modes))
                 (or company-dabbrev-code-everywhere
                     (not (company-in-string-or-comment)))
                 (or (company-grab-symbol) 'stop)))
    (candidates (let ((case-fold-search company-dabbrev-code-ignore-case))
                  (company-dabbrev--search
                   (company-dabbrev-code--make-regexp arg)
                   company-dabbrev-code-time-limit
                   company-dabbrev-code-other-buffers t)))
    (ignore-case company-dabbrev-code-ignore-case)
    (duplicates t)))

(provide 'company-dabbrev-code)
;;; company-dabbrev-code.el ends here
