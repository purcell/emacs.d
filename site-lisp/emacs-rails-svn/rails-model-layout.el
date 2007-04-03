;;; rails-model-layout.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-model-layout.el $
;; $Id: rails-model-layout.el 158 2007-04-03 08:45:46Z dimaexe $

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

(defun rails-model-layout:keymap (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (map (make-sparse-keymap))
         (menubar (make-sparse-keymap))
         (model (rails-core:current-model))
         (controller (pluralize-string model)))
    (when type
      (unless (rails-core:mailer-p model)
        (unless (eq type :model)
          (define-key menubar [go-to-model]      '("Go to Model" . rails-model-layout:switch-to-model))
          (define-key map (kbd "\C-c m")         'rails-model-layout:switch-to-model))
        (unless (eq type :unit-test)
          (define-key menubar [go-to-unit-test]  '("Go to Unit Test" . rails-model-layout:switch-to-unit-test))
          (define-key map (kbd "\C-c u")         'rails-model-layout:switch-to-unit-test))
        (when (and (not (eq type :migration))
                   (rails-core:migration-file (format "Create%s" controller)))
          (define-key menubar [go-to-migration]  '("Go to Migration" . rails-model-layout:switch-to-migration))
          (define-key map (kbd "\C-c g")         'rails-model-layout:switch-to-migration))
        (when (and (not (eq type :controller))
                   (rails-core:controller-exist-p controller))
          (define-key menubar [go-to-controller] '("Go to Controller" . rails-model-layout:switch-to-controller))
          (define-key map (kbd "\C-c c")         'rails-model-layout:switch-to-controller))
        (unless (eq type :fixture)
          (define-key menubar [go-to-fixture]    '("Go to Fixture" . rails-model-layout:switch-to-fixture))
          (define-key map (kbd "\C-c x")         'rails-model-layout:switch-to-fixture)))
      (when (rails-core:mailer-p model)
        (define-key menubar [go-to-mailer] '("Go to Mailer" . rails-model-layout:switch-to-mailer))
        (define-key map (kbd "\C-c n")         'rails-model-layout:switch-to-mailer))
      (define-key menubar [sep] (rails-core:menu-separator))
      (define-key menubar [primary-switch] '("Switch to related" . rails-lib:run-primary-switch))
      (define-key menubar [secondary-switch] '("Switch to related with menu" . rails-lib:run-secondary-switch))
      (define-key map [menu-bar rails-model-layout] (cons name menubar)))
    map))

(defun rails-model-layout:switch-to (type)
  (let* ((model (rails-core:current-model))
         (controller (rails-core:current-controller))
         (item (if controller controller model))
         (item (case type
                 (:mailer (rails-core:mailer-file model))
                 (:controller (rails-core:controller-file (pluralize-string model)))
                 (:fixture (rails-core:fixture-file model))
                 (:unit-test (rails-core:unit-test-file item))
                 (:model (rails-core:model-file model))
                 (:migration (rails-core:migration-file (concat "Create" (pluralize-string model)))))))
    (when item
      (let ((file (rails-core:file item)))
        (if (file-exists-p file)
            (progn
              (find-file file)
              (message (format "%s: %s" (substring (symbol-name type) 1) item)))
          (message "File %s not exists" file))))))

(defun rails-model-layout:switch-to-mailer () (interactive) (rails-model-layout:switch-to :mailer))
(defun rails-model-layout:switch-to-controller () (interactive) (rails-model-layout:switch-to :controller))
(defun rails-model-layout:switch-to-fixture () (interactive) (rails-model-layout:switch-to :fixture))
(defun rails-model-layout:switch-to-unit-test () (interactive) (rails-model-layout:switch-to :unit-test))
(defun rails-model-layout:switch-to-model () (interactive) (rails-model-layout:switch-to :model))
(defun rails-model-layout:switch-to-migration () (interactive) (rails-model-layout:switch-to :migration))

(defun rails-model-layout:menu ()
  (interactive)
  (let* ((item (list))
         (type (rails-core:buffer-type))
         (title (capitalize (substring (symbol-name type) 1)))
         (model (rails-core:current-model))
         (controller (pluralize-string model)))
    (unless (rails-core:mailer-p model)
      (when (and (not (eq type :migration))
                 (rails-core:migration-file (format
                                             "Create%s"
                                             (pluralize-string model))))
        (add-to-list 'item (cons "Migration" :migration)))
      (unless (eq type :fixture)
        (add-to-list 'item (cons "Fixture" :fixture)))
      (when (rails-core:controller-exist-p controller)
        (add-to-list 'item (cons "Controller" :controller)))
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