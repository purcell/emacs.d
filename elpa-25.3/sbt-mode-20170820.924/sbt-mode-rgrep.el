;;; sbt-mode-rgrep.el - Functions for searching within an sbt project
;;
;; Copyright(c) 2013 Heikki Vesalainen

;;; Parts of this file are based on grep.el from Emacs. The following
;;; copyright statement applies to those parts.

;; Copyright (C) 1985-1987, 1993-1999, 2001-2012
;;   Free Software Foundation, Inc.

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

(require 'sbt-mode-vars)
(require 'sbt-mode-project)

(require 'grep)

(defun sbt:regexp-for-id (id)
  (if (fboundp 'scala-syntax:regexp-for-id)
      (funcall 'scala-syntax:regexp-for-id id)
    (concat "\\b" id "\\b")))

(defun sbt:verify-defaults-for-rgrep ()
  (grep-compute-defaults)
  (unless grep-find-template
    (error "sbt-mode-rgrep.el: No `grep-find-template' available"))
  (unless (sbt:find-root)
    (error "sbt-mode-rgrep.el: Could not find sbt project root, see `C-h f sbt:find-root` for help.")))

;;;###autoload
(defun sbt-grep (regexp &optional files dir confirm)
  "Recursively grep for REGEXP in FILES in directory tree rooted at DIR. By default DIR is is the sbt project root."
  (interactive
   (progn
     (sbt:verify-defaults-for-rgrep)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " grep-find-command
                                 nil nil 'grep-find-history)))
      (t (let* ((regexp (grep-read-regexp))
                (files (grep-read-files regexp))
                (dir (read-directory-name "Base directory: " (sbt:find-root) nil t))
                (confirm (equal current-prefix-arg '(4))))
         (list regexp files dir confirm))))))
  (let ((default-directory (sbt:find-root)))
    (rgrep regexp files dir confirm)))

;;;###autoload
(defun sbt-find-usages (id &optional dir confirm)
  "Recursively grep for ID in scala files in directory tree rooted at DIR. By default DIR is is the sbt project root."
  (interactive
   (progn
     (sbt:verify-defaults-for-rgrep)
     (let* ((regexp (grep-read-regexp))
            (dir (read-directory-name "Base directory: " (sbt:find-root) nil t))
            (confirm (equal current-prefix-arg '(4))))
       (list regexp dir confirm))))
  (rgrep (concat (sbt:regexp-for-id id)) "*.scala *.java" dir confirm))

;;;###autoload
(defun sbt-find-definitions (id &optional confirm)
  "Recursively grep for definition of ID in scala files in the directory tree rooted at the sbt project root."
  (interactive
   (progn
     (sbt:verify-defaults-for-rgrep)
     (list (grep-read-regexp) (equal current-prefix-arg '(4)))))
  (let ((grep-setup-hook (copy-sequence grep-setup-hook))) ; let-bind a copy of the hook
    (add-hook 'grep-setup-hook 'sbt:grep-setup-function)
    (rgrep (concat "\\(class\\|type\\|trait\\|object\\|va[rl]\\|def\\|package\\)[ \\t]\\+" (sbt:regexp-for-id id)) "*.scala *.java" (sbt:find-root) confirm)))

(defun sbt:grep-setup-function ()
  (setq-local compilation-auto-jump-to-first-error t)
  (setq-local compilation-auto-jump-to-next t))

(provide 'sbt-mode-rgrep)
