;;; rails-for-functional-test.el ---

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-for-rhtml.el $
;; $Id: rails-for-rhtml.el 58 2006-12-17 21:47:39Z dimaexe $

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

(defun rails-for-functional-test:switch-to-controller ()
  (interactive)
  (rails-core:find-file-if-exist (rails-core:controller-file (rails-core:current-controller))))

(defun rails-for-functional-test:switch-with-menu ()
  (interactive)
  (let ((menu (rails-core:menu-of-views (rails-core:current-controller) t))
        (helper (rails-core:file (rails-core:helper-file (rails-core:current-controller))))
        (controller (rails-core:file (rails-core:controller-file (rails-core:current-controller))))
        item)
    (when (file-exists-p helper)
      (add-to-list 'menu (list "Helper" helper)))
    (when (file-exists-p controller)
      (add-to-list 'menu (list "Controller" controller)))
    (setq item
          (rails-core:menu
           (list (concat "Functional Test "
                         (rails-core:current-controller)
                         "Test")
                 (cons "Please select.." menu))))
    (when (and item (file-exists-p item))
      (find-file item))))

(defun rails-for-functional-test ()
  "Enable Functional test configurations."
  (interactive)
  (setq rails-primary-switch-func 'rails-for-functional-test:switch-to-controller)
  (setq rails-secondary-switch-func 'rails-for-functional-test:switch-with-menu))

(provide 'rails-for-functional-test)