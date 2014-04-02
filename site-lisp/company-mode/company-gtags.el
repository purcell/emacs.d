;;; company-gtags.el --- company-mode completion back-end for GNU Global

;; Copyright (C) 2009-2011  Free Software Foundation, Inc.

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
(eval-when-compile (require 'cl))

(defgroup company-gtags nil
  "Completion back-end for GNU Global."
  :group 'company)

(defcustom company-gtags-executable
  (executable-find "global")
  "Location of GNU global executable."
  :type 'string)

(define-obsolete-variable-alias
  'company-gtags-gnu-global-program-name
  'company-gtags-executable "earlier")

(defvar company-gtags--tags-available-p 'unknown)
(make-variable-buffer-local 'company-gtags--tags-available-p)

(defvar company-gtags-modes '(c-mode c++-mode jde-mode java-mode php-mode))

(defun company-gtags--tags-available-p ()
  (if (eq company-gtags--tags-available-p 'unknown)
      (setq company-gtags--tags-available-p
            (company-locate-dominating-file buffer-file-name "GTAGS"))
    company-gtags--tags-available-p))

(defun company-gtags-fetch-tags (prefix)
  (with-temp-buffer
    (let (tags)
      (when (= 0 (call-process company-gtags-executable nil
                               (list (current-buffer) nil) nil "-c" prefix))
        (goto-char (point-min))
        (split-string (buffer-string) "\n" t)))))

(defun company-gtags-location (tag)
  (with-temp-buffer
    (when (= 0 (call-process company-gtags-executable nil
                             (list (current-buffer) nil) nil "-x" tag))
        (goto-char (point-min))
        (when (looking-at (concat (regexp-quote tag)
                                  "[ \t]+\\([[:digit:]]+\\)"
                                  "[ \t]+\\([^ \t]+\\)"))
          (cons (expand-file-name (match-string 2))
                (string-to-number (match-string 1)))))))

;;;###autoload
(defun company-gtags (command &optional arg &rest ignored)
  "`company-mode' completion back-end for GNU Global."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-gtags))
    (prefix (and company-gtags-executable
                 (memq major-mode company-gtags-modes)
                 (not (company-in-string-or-comment))
                 (company-gtags--tags-available-p)
                 (or (company-grab-symbol) 'stop)))
    (candidates (company-gtags-fetch-tags arg))
    (sorted t)
    (location (company-gtags-location arg))))

(provide 'company-gtags)
;;; company-gtags.el ends here
