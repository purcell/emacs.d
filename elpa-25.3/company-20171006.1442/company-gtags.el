;;; company-gtags.el --- company-mode completion backend for GNU Global

;; Copyright (C) 2009-2011, 2014  Free Software Foundation, Inc.

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
(require 'company-template)
(require 'cl-lib)

(defgroup company-gtags nil
  "Completion backend for GNU Global."
  :group 'company)

(defcustom company-gtags-executable
  (executable-find "global")
  "Location of GNU global executable."
  :type 'string)

(define-obsolete-variable-alias
  'company-gtags-gnu-global-program-name
  'company-gtags-executable "earlier")

(defcustom company-gtags-insert-arguments t
  "When non-nil, insert function arguments as a template after completion."
  :type 'boolean
  :package-version '(company . "0.8.1"))

(defvar-local company-gtags--tags-available-p 'unknown)

(defcustom company-gtags-modes '(prog-mode jde-mode)
  "Modes that use `company-gtags'.
In all these modes (and their derivatives) `company-gtags' will perform
completion."
  :type '(repeat (symbol :tag "Major mode"))
  :package-version '(company . "0.8.4"))

(defun company-gtags--tags-available-p ()
  (if (eq company-gtags--tags-available-p 'unknown)
      (setq company-gtags--tags-available-p
            (locate-dominating-file buffer-file-name "GTAGS"))
    company-gtags--tags-available-p))

(defun company-gtags--fetch-tags (prefix)
  (with-temp-buffer
    (let (tags)
      (when (= 0 (process-file company-gtags-executable nil
                               ;; "-T" goes through all the tag files listed in GTAGSLIBPATH
                               (list (current-buffer) nil) nil "-xGqT" (concat "^" prefix)))
        (goto-char (point-min))
        (cl-loop while
                 (re-search-forward (concat
                                     "^"
                                     "\\([^ ]*\\)" ;; completion
                                     "[ \t]+\\([[:digit:]]+\\)" ;; linum
                                     "[ \t]+\\([^ \t]+\\)" ;; file
                                     "[ \t]+\\(.*\\)" ;; definition
                                     "$"
                                     ) nil t)
                 collect
                 (propertize (match-string 1)
                             'meta (match-string 4)
                             'location (cons (expand-file-name (match-string 3))
                                             (string-to-number (match-string 2)))
                             ))))))

(defun company-gtags--annotation (arg)
  (let ((meta (get-text-property 0 'meta arg)))
    (when (string-match (concat arg "\\((.*)\\).*") meta)
      (match-string 1 meta))))

;;;###autoload
(defun company-gtags (command &optional arg &rest ignored)
  "`company-mode' completion backend for GNU Global."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-gtags))
    (prefix (and company-gtags-executable
                 buffer-file-name
                 (apply #'derived-mode-p company-gtags-modes)
                 (not (company-in-string-or-comment))
                 (company-gtags--tags-available-p)
                 (or (company-grab-symbol) 'stop)))
    (candidates (company-gtags--fetch-tags arg))
    (sorted t)
    (duplicates t)
    (annotation (company-gtags--annotation arg))
    (meta (get-text-property 0 'meta arg))
    (location (get-text-property 0 'location arg))
    (post-completion (let ((anno (company-gtags--annotation arg)))
                       (when (and company-gtags-insert-arguments anno)
                         (insert anno)
                         (company-template-c-like-templatify anno))))))

(provide 'company-gtags)
;;; company-gtags.el ends here
