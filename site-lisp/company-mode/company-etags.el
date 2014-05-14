;;; company-etags.el --- company-mode completion back-end for etags

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

(eval-when-compile (require 'cl))
(require 'company)
(require 'etags)

(defgroup company-etags nil
  "Completion back-end for etags."
  :group 'company)

(defcustom company-etags-use-main-table-list t
  "Always search `tags-table-list' if set.
If this is disabled, `company-etags' will try to find the one table for each
buffer automatically."
  :type '(choice (const :tag "off" nil)
                 (const :tag "on" t)))

(defcustom company-etags-ignore-case nil
  "Non-nil to ignore case in completion candidates."
  :type 'boolean)

(defvar company-etags-modes '(prog-mode c-mode objc-mode c++-mode java-mode
                              jde-mode pascal-mode perl-mode python-mode))

(defvar company-etags-buffer-table 'unknown)
(make-variable-buffer-local 'company-etags-buffer-table)

(defun company-etags-find-table ()
  (let ((file (company-locate-dominating-file (or buffer-file-name
                                                  default-directory)
                                              "TAGS")))
    (when file
      (list (expand-file-name file)))))

(defun company-etags-buffer-table ()
  (or (and company-etags-use-main-table-list tags-table-list)
      (if (eq company-etags-buffer-table 'unknown)
          (setq company-etags-buffer-table (company-etags-find-table))
        company-etags-buffer-table)))

(defun company-etags--candidates (prefix)
  (let ((tags-table-list (company-etags-buffer-table))
        (completion-ignore-case company-etags-ignore-case))
    (and (or tags-file-name tags-table-list)
         (fboundp 'tags-completion-table)
         (save-excursion
           (visit-tags-table-buffer)
           (all-completions prefix (tags-completion-table))))))

;;;###autoload
(defun company-etags (command &optional arg &rest ignored)
  "`company-mode' completion back-end for etags."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-etags))
    (prefix (and (apply 'derived-mode-p company-etags-modes)
                 (not (company-in-string-or-comment))
                 (company-etags-buffer-table)
                 (or (company-grab-symbol) 'stop)))
    (candidates (company-etags--candidates arg))
    (location (let ((tags-table-list (company-etags-buffer-table)))
                (when (fboundp 'find-tag-noselect)
                  (save-excursion
                    (let ((buffer (find-tag-noselect arg)))
                      (cons buffer (with-current-buffer buffer (point))))))))
    (ignore-case company-etags-ignore-case)))

(provide 'company-etags)
;;; company-etags.el ends here
