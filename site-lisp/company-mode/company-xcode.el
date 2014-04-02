;;; company-xcode.el --- company-mode completion back-end for Xcode projects

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

(defgroup company-xcode nil
  "Completion back-end for Xcode projects."
  :group 'company)

(defcustom company-xcode-xcodeindex-executable (executable-find "xcodeindex")
  "Location of xcodeindex executable."
  :type 'file)

(defvar company-xcode-tags nil)

(defun company-xcode-reset ()
  "Reset the cached tags."
  (interactive)
  (setq company-xcode-tags nil))

(defcustom company-xcode-types
  '("Class" "Constant" "Enum" "Macro" "Modeled Class" "Structure"
    "Type" "Union" "Function")
  "The types of symbols offered by `company-xcode'.
No context-enabled completion is available.  Types like methods will be
offered regardless of whether the class supports them.  The defaults should be
valid in most contexts."
  :set (lambda (variable value)
         (set variable value)
         (company-xcode-reset))
  :type '(set (const "Category") (const "Class") (const "Class Method")
              (const "Class Variable") (const "Constant") (const "Enum")
              (const "Field") (const "Instance Method")
              (const "Instance Variable") (const "Macro")
              (const "Modeled Class") (const "Modeled Method")
              (const "Modeled Property") (const "Property") (const "Protocol")
              (const "Structure") (const "Type") (const "Union")
              (const "Variable") (const "Function")))

(defvar company-xcode-project 'unknown)
(make-variable-buffer-local 'company-xcode-project)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-xcode-fetch (project-bundle)
  (setq project-bundle (directory-file-name project-bundle))
  (message "Retrieving dump from %s..." project-bundle)
  (with-temp-buffer
    (let ((default-directory (file-name-directory project-bundle)))
      (call-process company-xcode-xcodeindex-executable nil (current-buffer)
                    nil "dump" "-project"
                    (file-name-nondirectory project-bundle) "-quiet")
      (goto-char (point-min))
      (let ((regexp (concat "^\\([^\t\n]*\\)\t[^\t\n]*\t"
                            (regexp-opt company-xcode-types)
                            "\t[^\t\n]*\t[^\t\n]*"))
            candidates)
        (while (re-search-forward regexp nil t)
          (add-to-list 'candidates (match-string 1)))
        (message "Retrieving dump from %s...done" project-bundle)
        candidates))))

(defun company-xcode-find-project ()
  (let ((dir (if buffer-file-name
                 (file-name-directory buffer-file-name)
               (expand-file-name default-directory)))
        (prev-dir nil)
        file)
    (while (not (or file (equal dir prev-dir)))
      (setq file (car (directory-files dir t ".xcodeproj\\'" t))
            prev-dir dir
            dir (file-name-directory (directory-file-name dir))))
    file))

(defun company-xcode-tags ()
  (when (eq company-xcode-project 'unknown)
    (setq company-xcode-project (company-xcode-find-project)))
  (when company-xcode-project
    (cdr (or (assoc company-xcode-project company-xcode-tags)
             (car (push (cons company-xcode-project
                              (company-xcode-fetch company-xcode-project))
                        company-xcode-tags))))))
;;;###autoload
(defun company-xcode (command &optional arg &rest ignored)
  "`company-mode' completion back-end for Xcode projects."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-xcode))
    (prefix (and company-xcode-xcodeindex-executable
                 (company-xcode-tags)
                 (not (company-in-string-or-comment))
                 (or (company-grab-symbol) 'stop)))
    (candidates (let ((completion-ignore-case nil))
                  (company-xcode-tags)
                  (all-completions arg (company-xcode-tags))))))


(provide 'company-xcode)
;;; company-xcode.el ends here
