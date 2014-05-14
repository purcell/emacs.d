;;; company-semantic.el --- company-mode completion back-end using Semantic

;; Copyright (C) 2009-2011, 2013  Free Software Foundation, Inc.

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

(defvar semantic-idle-summary-function)
(declare-function semantic-documentation-for-tag "semantic/doc" )
(declare-function semantic-analyze-current-context "semantic/analyze")
(declare-function semantic-analyze-possible-completions "semantic/complete")
(declare-function semantic-analyze-find-tags-by-prefix "semantic/analyze/fcn")
(declare-function semantic-tag-class "semantic/tag")
(declare-function semantic-tag-name "semantic/tag")
(declare-function semantic-tag-start "semantic/tag")
(declare-function semantic-tag-buffer "semantic/tag")
(declare-function semantic-active-p "semantic")

(defgroup company-semantic nil
  "Completion back-end using Semantic."
  :group 'company)

(defcustom company-semantic-metadata-function 'company-semantic-summary-and-doc
  "The function turning a semantic tag into doc information."
  :type 'function)

(defvar company-semantic-modes '(c-mode c++-mode jde-mode java-mode))

(defvar company-semantic--current-tags nil
  "Tags for the current context.")
(make-variable-buffer-local 'company-semantic--current-tags)

(defun company-semantic-documentation-for-tag (tag)
  (when (semantic-tag-buffer tag)
    ;; When TAG's buffer is unknown, the function below raises an error.
    (semantic-documentation-for-tag tag)))

(defun company-semantic-doc-or-summary (tag)
  (or (company-semantic-documentation-for-tag tag)
      (and (require 'semantic-idle nil t)
           (require 'semantic/idle nil t)
           (funcall semantic-idle-summary-function tag nil t))))

(defun company-semantic-summary-and-doc (tag)
  (let ((doc (company-semantic-documentation-for-tag tag))
        (summary (funcall semantic-idle-summary-function tag nil t)))
    (and (stringp doc)
         (string-match "\n*\\(.*\\)$" doc)
         (setq doc (match-string 1 doc)))
    (concat summary
            (when doc
                  (if (< (+ (length doc) (length summary) 4) (window-width))
                      " -- "
                    "\n"))
            doc)))

(defun company-semantic-doc-buffer (tag)
  (let ((doc (company-semantic-documentation-for-tag tag)))
    (when doc
      (company-doc-buffer
       (concat (funcall semantic-idle-summary-function tag nil t)
               "\n"
               doc)))))

(defsubst company-semantic-completions (prefix)
  (ignore-errors
    (let ((completion-ignore-case nil)
          (context (semantic-analyze-current-context)))
      (setq company-semantic--current-tags
            (semantic-analyze-possible-completions context))
      (all-completions prefix company-semantic--current-tags))))

(defun company-semantic-completions-raw (prefix)
  (setq company-semantic--current-tags nil)
  (dolist (tag (semantic-analyze-find-tags-by-prefix prefix))
    (unless (eq (semantic-tag-class tag) 'include)
      (push tag company-semantic--current-tags)))
  (delete "" (mapcar 'semantic-tag-name company-semantic--current-tags)))

(defun company-semantic--pre-prefix-length (prefix-length)
  "Sum up the length of all chained symbols before POS.
Symbols are chained by \".\" or \"->\"."
  (save-excursion
    (let ((pos (point)))
      (goto-char (- (point) prefix-length))
      (while (looking-back "->\\|\\.")
        (goto-char (match-beginning 0))
        (skip-syntax-backward "w_"))
      (- pos (point)))))

(defun company-semantic--grab ()
  "Grab the semantic prefix, but return everything before -> or . as length."
  (let ((symbol (company-grab-symbol)))
    (when symbol
      (cons symbol (company-semantic--pre-prefix-length (length symbol))))))

;;;###autoload
(defun company-semantic (command &optional arg &rest ignored)
  "`company-mode' completion back-end using CEDET Semantic."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-semantic))
    (prefix (and (featurep 'semantic)
                 (semantic-active-p)
                 (memq major-mode company-semantic-modes)
                 (not (company-in-string-or-comment))
                 (or (company-semantic--grab) 'stop)))
    (candidates (if (and (equal arg "")
                         (not (looking-back "->\\|\\.")))
                    (company-semantic-completions-raw arg)
                  (company-semantic-completions arg)))
    (meta (funcall company-semantic-metadata-function
                   (assoc arg company-semantic--current-tags)))
    (doc-buffer (company-semantic-doc-buffer
                 (assoc arg company-semantic--current-tags)))
    ;; Because "" is an empty context and doesn't return local variables.
    (no-cache (equal arg ""))
    (location (let ((tag (assoc arg company-semantic--current-tags)))
                (when (buffer-live-p (semantic-tag-buffer tag))
                  (cons (semantic-tag-buffer tag)
                        (semantic-tag-start tag)))))))

(provide 'company-semantic)
;;; company-semantic.el ends here
