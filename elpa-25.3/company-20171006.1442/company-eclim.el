;;; company-eclim.el --- company-mode completion backend for Eclim

;; Copyright (C) 2009, 2011, 2013, 2015  Free Software Foundation, Inc.

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
;; Using `emacs-eclim' together with (or instead of) this backend is
;; recommended, as it allows you to use other Eclim features.
;;
;; The alternative backend provided by `emacs-eclim' uses `yasnippet'
;; instead of `company-template' to expand function calls, and it supports
;; some languages other than Java.

;;; Code:

(require 'company)
(require 'company-template)
(require 'cl-lib)

(defgroup company-eclim nil
  "Completion backend for Eclim."
  :group 'company)

(defun company-eclim-executable-find ()
  (let (file)
    (cl-dolist (eclipse-root '("/Applications/eclipse" "/usr/lib/eclipse"
                            "/usr/local/lib/eclipse"))
      (and (file-exists-p (setq file (expand-file-name "plugins" eclipse-root)))
           (setq file (car (last (directory-files file t "^org.eclim_"))))
           (file-exists-p (setq file (expand-file-name "bin/eclim" file)))
           (cl-return file)))))

(defcustom company-eclim-executable
  (or (bound-and-true-p eclim-executable)
      (executable-find "eclim")
      (company-eclim-executable-find))
  "Location of eclim executable."
  :type 'file)

(defcustom company-eclim-auto-save t
  "Determines whether to save the buffer when retrieving completions.
eclim can only complete correctly when the buffer has been saved."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "On" t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-local company-eclim--project-dir 'unknown)

(defvar-local company-eclim--project-name nil)

(declare-function json-read "json")
(defvar json-array-type)

(defun company-eclim--call-process (&rest args)
  (let ((coding-system-for-read 'utf-8)
        res)
    (require 'json)
    (with-temp-buffer
      (if (= 0 (setq res (apply 'call-process company-eclim-executable nil t nil
                                "-command" args)))
          (let ((json-array-type 'list))
            (goto-char (point-min))
            (unless (eobp)
              (json-read)))
        (message "Company-eclim command failed with error %d:\n%s" res
                 (buffer-substring (point-min) (point-max)))
        nil))))

(defun company-eclim--project-list ()
  (company-eclim--call-process "project_list"))

(defun company-eclim--project-dir ()
  (if (eq company-eclim--project-dir 'unknown)
      (let ((dir (locate-dominating-file buffer-file-name ".project")))
        (when dir
          (setq company-eclim--project-dir
                (directory-file-name
                 (expand-file-name dir)))))
    company-eclim--project-dir))

(defun company-eclim--project-name ()
  (or company-eclim--project-name
      (let ((dir (company-eclim--project-dir)))
        (when dir
          (setq company-eclim--project-name
                (cl-loop for project in (company-eclim--project-list)
                         when (equal (cdr (assoc 'path project)) dir)
                         return (cdr (assoc 'name project))))))))

(defun company-eclim--candidates (prefix)
  (interactive "d")
  (let ((project-file (file-relative-name buffer-file-name
                                          (company-eclim--project-dir)))
        completions)
    (when company-eclim-auto-save
      (when (buffer-modified-p)
        (basic-save-buffer))
      ;; FIXME: Sometimes this isn't finished when we complete.
      (company-eclim--call-process "java_src_update"
                                   "-p" (company-eclim--project-name)
                                   "-f" project-file))
    (dolist (item (cdr (assoc 'completions
                              (company-eclim--call-process
                               "java_complete" "-p" (company-eclim--project-name)
                               "-f" project-file
                               "-o" (number-to-string
                                     (company-eclim--search-point prefix))
                               "-e" "utf-8"
                               "-l" "standard"))))
      (let* ((meta (cdr (assoc 'info item)))
             (completion meta))
        (when (string-match " ?[(:-]" completion)
          (setq completion (substring completion 0 (match-beginning 0))))
        (put-text-property 0 1 'meta meta completion)
        (push completion completions)))
    (let ((completion-ignore-case nil))
      (all-completions prefix completions))))

(defun company-eclim--search-point (prefix)
  (if (or (cl-plusp (length prefix)) (eq (char-before) ?.))
      (1- (point))
    (point)))

(defun company-eclim--meta (candidate)
  (get-text-property 0 'meta candidate))

(defun company-eclim--annotation (candidate)
  (let ((meta (company-eclim--meta candidate)))
    (when (string-match "\\(([^-]*\\) -" meta)
      (substring meta (match-beginning 1) (match-end 1)))))

(defun company-eclim--prefix ()
  (let ((prefix (company-grab-symbol)))
    (when prefix
      ;; Completion candidates for annotations don't include '@'.
      (when (eq ?@ (string-to-char prefix))
        (setq prefix (substring prefix 1)))
      prefix)))

(defun company-eclim (command &optional arg &rest ignored)
  "`company-mode' completion backend for Eclim.
Eclim provides access to Eclipse Java IDE features for other editors.

Eclim version 1.7.13 or newer (?) is required.

Completions only work correctly when the buffer has been saved.
`company-eclim-auto-save' determines whether to do this automatically."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-eclim))
    (prefix (and (derived-mode-p 'java-mode 'jde-mode)
                 buffer-file-name
                 company-eclim-executable
                 (company-eclim--project-name)
                 (not (company-in-string-or-comment))
                 (or (company-eclim--prefix) 'stop)))
    (candidates (company-eclim--candidates arg))
    (meta (company-eclim--meta arg))
    ;; because "" doesn't return everything
    (no-cache (equal arg ""))
    (annotation (company-eclim--annotation arg))
    (post-completion (let ((anno (company-eclim--annotation arg)))
                       (when anno
                         (insert anno)
                         (company-template-c-like-templatify anno))))))

(provide 'company-eclim)
;;; company-eclim.el ends here
