;;; direx-project.el --- Project Module for Direx

;; Copyright (C) 2012  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <tomo@cx4a.org>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl)
(require 'direx)

(defgroup direx-project nil
  "Project Module for Direx."
  :group 'direx
  :prefix "direx-project:")

(defcustom direx-project:project-root-predicate-functions
  '(direx-project:vc-root-p)
  "The list of functions which predicate whether the directory is
a project root or not."
  :type '(repeat function)
  :group 'direx-project)

(defun direx-project:vc-root-p (dirname)
  (loop for vc-dir in '(".git" ".hg" ".bzr")
        thereis (file-exists-p (expand-file-name vc-dir dirname))))

(defun direx-project:project-root-p (dirname)
  (some (lambda (fun) (funcall fun dirname))
        direx-project:project-root-predicate-functions))

(defun direx-project:find-project-root-noselect (filename)
  (interactive)
  (loop for parent-dirname in (if (file-directory-p filename)
                                  (cons filename
                                        (direx:directory-parents filename))
                                (direx:directory-parents filename))
        if (direx-project:project-root-p parent-dirname)
        return (direx:find-directory-noselect parent-dirname)))

(defun direx-project:jump-to-project-root-noselect ()
  (interactive)
  (let ((buffer (direx-project:find-project-root-noselect
                 (or buffer-file-name default-directory))))
    (if buffer
        (direx:maybe-goto-current-buffer-item buffer)
      (error "Project root not found"))
    buffer))

;;;###autoload
(defun direx-project:jump-to-project-root ()
  (interactive)
  (switch-to-buffer (direx-project:jump-to-project-root-noselect)))

;;;###autoload
(defun direx-project:jump-to-project-root-other-window ()
  (interactive)
  (switch-to-buffer-other-window (direx-project:jump-to-project-root-noselect)))

(provide 'direx-project)
;;; direx-project.el ends here
