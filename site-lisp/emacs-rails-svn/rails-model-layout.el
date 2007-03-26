;;; rails-model-layout.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-model-layout.el $
;; $Id: rails-model-layout.el 112 2007-03-24 22:34:38Z dimaexe $

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

(defun rails-model-layout:switch-to (type)
  (let* ((model (rails-core:current-model))
         (item (case type
                 (:mailer (rails-core:mailer-file model))
                 (:controller (rails-core:controller-file (pluralize-string model)))
                 (:fixture (rails-core:fixture-file model))
                 (:unit-test (rails-core:unit-test-file model))
                 (:model (rails-core:model-file model)))))
    (rails-core:find-file-if-exist item)
    (message (format "%s: %s" (substring (symbol-name type) 1) model))))

(defun rails-model-layout:menu ()
  (interactive)
  (let* ((item (list))
         (type (rails-core:buffer-type))
         (title (capitalize (substring (symbol-name type) 1)))
         (model (rails-core:current-model))
         (controller (pluralize-string model)))
    (unless (rails-core:mailer-p model)
      (when (rails-core:controller-exist-p controller)
        (add-to-list 'item (cons "Controller" :controller)))
      (unless (eq type :fixture)
        (add-to-list 'item (cons "Fixture" :fixture)))
      (unless (eq type :unit-test)
        (add-to-list 'item (cons "Unit test" :unit-test)))
      (unless (eq type :model)
        (add-to-list 'item (cons "Model" :model))))
    (when (rails-core:mailer-p model)
      (setq item (rails-controller-layout:views-menu model))
      (add-to-list 'item (rails-core:menu-separator))
      (add-to-list 'item (cons "Mailer" :mailer)))
    (setq item
          (rails-core:menu
           (list (concat title " " model)
                 (cons "Please select.."
                       item))))
    (typecase item
      (symbol (rails-model-layout:switch-to item))
      (string (rails-core:find-file-if-exist item)))))

(provide 'rails-model-layout)