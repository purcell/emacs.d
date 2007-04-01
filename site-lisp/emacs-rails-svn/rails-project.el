;;; rails-project.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails.el $
;; $Id: rails.el 149 2007-03-29 15:07:49Z dimaexe $

;;; License

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Code:

(defun rails-project:root ()
  "Return RAILS_ROOT if this file is a part of a Rails application,
else return nil"
  (let ((curdir default-directory)
        (max 10)
        (found nil))
    (while (and (not found) (> max 0))
      (progn
        (if (file-exists-p (concat curdir "config/environment.rb"))
            (progn
              (setq found t))
          (progn
            (setq curdir (concat curdir "../"))
            (setq max (- max 1))))))
    (if found (expand-file-name curdir))))

(defmacro* rails-project:with-root ((root) &body body)
  "If you use `rails-project:root' or functions related on it
several times in a block of code, you can optimize your code by
using this macro. Also, blocks of code will be executed only if
rails-root exist.
 (rails-project:with-root (root)
    (foo root)
    (bar (rails-core:file \"some/path\")))
 "
 `(let ((,root (rails-project:root)))
    (when ,root
      (flet ((rails-project:root () ,root))
        ,@body))))

(defmacro rails-project:in-root (&rest body)
  "Set the default directory to the Rails root directory while
BODY is executed."
  (let ((root (gensym)))
    `(rails-project:with-root
      (,root)
      (let ((default-dir ,root))
        ,@body))))

(defun rails-project:name ()
  "Return the name of current Rails project."
  (replace-regexp-in-string "^.*/\\(.*\\)/$" "\\1"
          (directory-name (rails-project:root))))

(provide 'rails-project)