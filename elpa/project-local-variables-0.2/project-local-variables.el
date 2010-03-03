;;; project-local-variables.el --- set project-local variables from a file

;; Copyright (C) 2008 Ryan Davis and Phil Hagelberg

;; Author: Ryan Davis and Phil Hagelberg
;; URL: http://www.emacswiki.org/cgi-bin/wiki/ProjectLocalVariables
;; Version: 0.2
;; Created: 2008-03-18
;; Keywords: project, convenience
;; EmacsWiki: ProjectLocalVariables

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file allows you to create .emacs-project files that are
;; evaluated when a file is opened. You may also create
;; .emacs-project-$MODE files that only get loaded when you open files
;; of a specific mode in the project. All files for which a
;; .emacs-project file exists in an ancestor directory will have it
;; loaded.

;; It has not been tested in versions of Emacs prior to 22.

(defvar plv-project-file ".emacs-project"
  "Name prefix for project files.
 Emacs appends name of major mode and looks for such a file in
 the current directory and its parents.")

(defmacro setl (sym val)
  "Like setq, but makes sym a local variable first."
  `(set (make-local-variable ',sym) ,val))

;;;###autoload
(defun plv-find-project-file (dir mode-name)
  "Look up the project file in and above `dir'."
 (let ((f (expand-file-name (concat plv-project-file mode-name) dir))
       (parent (file-truename (expand-file-name ".." dir))))
   (cond ((string= dir parent) nil)
         ((file-exists-p f) f)
         (t (plv-find-project-file parent mode-name)))))

;;;###autoload
(defadvice hack-local-variables (before project-local-variables activate)
  "Load the appropriate .emacs-project files for a file."
  (let* ((full-name (symbol-name major-mode))
         (mode-name (if (string-match "\\(.*\\)-mode$" full-name)
                        (match-string 1 full-name)
                      full-name))
         (pfile (plv-find-project-file default-directory (concat "-" mode-name)))
         (gfile (plv-find-project-file default-directory "")))
    (save-excursion
      (when gfile (load gfile))
      (when pfile (load pfile)))))

(add-to-list 'auto-mode-alist '("^\.emacs-project" . emacs-lisp-mode))

(provide 'project-local-variables)
;;; project-local-variables.el ends here