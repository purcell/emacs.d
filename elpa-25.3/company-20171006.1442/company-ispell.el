;;; company-ispell.el --- company-mode completion backend using Ispell

;; Copyright (C) 2009-2011, 2013-2016  Free Software Foundation, Inc.

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
(require 'cl-lib)
(require 'ispell)

(defgroup company-ispell nil
  "Completion backend using Ispell."
  :group 'company)

(defcustom company-ispell-dictionary nil
  "Dictionary to use for `company-ispell'.
If nil, use `ispell-complete-word-dict'."
  :type '(choice (const :tag "default (nil)" nil)
                 (file :tag "dictionary" t)))

(defvar company-ispell-available 'unknown)

(defalias 'company-ispell--lookup-words
  (if (fboundp 'ispell-lookup-words)
      'ispell-lookup-words
    'lookup-words))

(defun company-ispell-available ()
  (when (eq company-ispell-available 'unknown)
    (condition-case err
        (progn
          (company-ispell--lookup-words "WHATEVER")
          (setq company-ispell-available t))
      (error
       (message "Company: ispell-look-command not found")
       (setq company-ispell-available nil))))
  company-ispell-available)

;;;###autoload
(defun company-ispell (command &optional arg &rest ignored)
  "`company-mode' completion backend using Ispell."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-ispell))
    (prefix (when (company-ispell-available)
              (company-grab-word)))
    (candidates
     (let ((words (company-ispell--lookup-words
                   arg
                   (or company-ispell-dictionary ispell-complete-word-dict)))
           (completion-ignore-case t))
       (if (string= arg "")
           ;; Small optimization.
           words
         ;; Work around issue #284.
         (all-completions arg words))))
    (sorted t)
    (ignore-case 'keep-prefix)))

(provide 'company-ispell)
;;; company-ispell.el ends here
