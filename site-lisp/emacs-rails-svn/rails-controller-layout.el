;;; rails-controller-layout.el ---

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-controller-layout.el $
;; $Id: rails-controller-layout.el 160 2007-04-03 10:37:49Z dimaexe $

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

(defvar rails-controller-layout:recent-template-type nil)

(defun rails-controller-layout:switch-to-action-in-controller (controller-name action-name)
  "Open CONTROLLER-NAME and go to ACTION-NAME."
  (if (or (rails-core:find-file-if-exist (rails-core:controller-file controller-name))
          (rails-core:find-file-if-exist (rails-core:mailer-file controller-name)))
      (progn
        (goto-char (point-min))
        (when action-name
          (if (search-forward-regexp (concat "^[ ]*def[ ]*" action-name) nil t)
              (recenter))
          (message (format "%s: %s" (substring (symbol-name (rails-core:buffer-type)) 1) controller-name))))))

(defun rails-controller-layout:switch-to-view (controller-name action-name)
  "Open the ACTION-NAME file for CONTROLLER-NAME in the views directory."
  (when action-name
    (let ((views (rails-controller-layout:view-files controller-name action-name))
          (title (substring (symbol-name (rails-core:buffer-type)) 1)))
      (cond
       ((= (length views) 1)
        (find-file (first views))
        (message "%s: %s#%s" title controller-name action-name))
       ((= (length views) 0)
        (rails-controller-layout:create-view-for-action controller-name action-name))))))

(defun rails-controller-layout:toggle-action-view ()
  (interactive)
  (let ((controller-name (rails-core:current-controller))
        (action-name (rails-core:current-action)))
    (case (rails-core:buffer-type)
      (:view
       (rails-controller-layout:switch-to-action-in-controller controller-name action-name))
      (:mailer
       (rails-controller-layout:switch-to-view controller-name action-name))
      (:controller
       (if action-name
           (rails-controller-layout:switch-to-view controller-name action-name)
         (rails-controller-layout:switch-to :functional-test))))))

(defun rails-controller-layout:create-view-for-action (controller-name action-name)
  (let ((type
         (if rails-controller-layout:recent-template-type
             rails-controller-layout:recent-template-type
           (car rails-templates-list))))
    (setq type
          (completing-read (format "View for %s#%s not found, create %s.[%s]? "
                                   controller-name action-name action-name type)
                           rails-templates-list
                           nil t type))
    (setq rails-controller-layout:recent-template-type type)
    (let ((file (rails-core:file (concat "app/views/"
                                         (replace-regexp-in-string "_controller" ""
                                                                   (rails-core:file-by-class controller-name t))))))
        (make-directory file t)
        (find-file (format "%s/%s.%s" file action-name type)))))

(defun rails-controller-layout:view-files (controller-name &optional action)
  "Retun a list containing the view file for CONTROLLER-NAME#ACTION.
If the action is nil, return all views for the controller."
  (rails-project:with-root
   (root)
   (directory-files
    (rails-core:file
     (rails-core:views-dir
      (rails-core:short-controller-name controller-name))) t
      (if action
          (concat "^" action (rails-core:regex-for-match-view))
        (rails-core:regex-for-match-view)))))

(defun rails-controller-layout:views-menu (controller-name)
  "Make menu of view for CONTROLLER-NAME."
  (let (menu)
    (setq menu
          (mapcar (lambda(i)
                    (list (concat (if (string-match "^_" (file-name-nondirectory i)) "Partial" "View")
                                  ": "
                                  (file-name-nondirectory i))
                          i))
                  (rails-controller-layout:view-files controller-name nil)))
    (when (zerop (length menu))
      (setq menu (list)))
    menu))

(defun rails-controller-layout:keymap (type)
  (let* ((name (capitalize (substring (symbol-name type) 1)))
         (map (make-sparse-keymap))
         (menubar (make-sparse-keymap))
         (controller (rails-core:current-controller))
         (model (singularize-string controller)))
    (when type
      (unless (eq type :mailer)
        (when (rails-core:model-exist-p model)
          (when (rails-core:migration-file (concat "Create" controller))
            (define-key menubar [go-to-migration]     '("Go to Migration" . rails-controller-layout:switch-to-migration))
            (define-key map (kbd "\C-c g")            'rails-controller-layout:switch-to-migration))
          (define-key menubar [go-to-model]           '("Go to Model" . rails-controller-layout:switch-to-model))
          (define-key map (kbd "\C-c m")              'rails-controller-layout:switch-to-model))
        (unless (eq type :helper)
          (define-key menubar [go-to-helper]          '("Go to Helper" . rails-controller-layout:switch-to-helper))
          (define-key map (kbd "\C-c h")              'rails-controller-layout:switch-to-helper))
        (unless (eq type :functional-test)
          (define-key menubar [go-to-functional-test] '("Go to Functional Test" . rails-controller-layout:switch-to-functional-test))
          (define-key map (kbd "\C-c f")              'rails-controller-layout:switch-to-functional-test))
        (unless (eq type :controller)
          (define-key menubar [go-to-controller]      '("Go to Controller" . rails-controller-layout:switch-to-controller))
          (define-key map (kbd "\C-c c")              'rails-controller-layout:switch-to-controller)))
      (when (eq type :mailer)
        (define-key menubar [go-to-unit-test] '("Go to Unit Test" . rails-model-layout:switch-to-unit-test))
        (define-key map (kbd "\C-c u")        'rails-model-layout:switch-to-unit-test))
      (define-key menubar [sep] (rails-core:menu-separator))
      (define-key menubar [primary-switch] '("Switch to related" . rails-lib:run-primary-switch))
      (define-key menubar [secondary-switch] '("Switch to related with menu" . rails-lib:run-secondary-switch))
      (define-key map [menu-bar rails-controller-layout] (cons name menubar)))
    map))

(defun rails-controller-layout:switch-to (type)
  (let* ((controller (rails-core:current-controller))
         (model (singularize-string controller))
         (item (case type
                 (:helper (rails-core:helper-file controller))
                 (:functional-test (rails-core:functional-test-file controller))
                 (:controller (rails-core:controller-file controller))
                 (:model (rails-core:model-file model))
                 (:migration (rails-core:migration-file (concat "Create" controller))))))
    (when item
      (let ((file (rails-core:file item)))
        (if (file-exists-p file)
            (progn
              (find-file file)
              (message (format "%s: %s" (substring (symbol-name type) 1) item)))
          (message "File %s not exists" file))))))

(defun rails-controller-layout:switch-to-helper () (interactive) (rails-controller-layout:switch-to :helper))
(defun rails-controller-layout:switch-to-functional-test () (interactive) (rails-controller-layout:switch-to :functional-test))
(defun rails-controller-layout:switch-to-controller () (interactive) (rails-controller-layout:switch-to :controller))
(defun rails-controller-layout:switch-to-model () (interactive) (rails-controller-layout:switch-to :model))
(defun rails-controller-layout:switch-to-migration () (interactive) (rails-controller-layout:switch-to :migration))

(defun rails-controller-layout:menu ()
  (interactive)
  (let* ((type (rails-core:buffer-type))
         (title (capitalize (substring (symbol-name type) 1)))
         (controller (rails-core:current-controller))
         (action (rails-core:current-action))
         (model (singularize-string controller))
         (item (rails-controller-layout:views-menu controller)))
    (add-to-list 'item (rails-core:menu-separator))
    (unless (rails-core:mailer-p controller)
      (when (rails-core:model-exist-p model)
        (when (rails-core:migration-file (concat "Create" controller))
          (add-to-list 'item (cons "Migration" :migration)))
        (add-to-list 'item (cons "Model" :model)))
      (unless (eq type :helper)
        (add-to-list 'item (cons "Helper" :helper)))
      (unless (eq type :functional-test)
        (add-to-list 'item (cons "Functional test" :functional-test)))
      (unless (eq type :controller)
        (add-to-list 'item (cons "Controller" :controller))))
    (when (rails-core:mailer-p controller)
      (add-to-list 'item (cons "Unit test" (rails-core:unit-test-file controller)))
      (when (eq type :view)
        (add-to-list 'item (cons "Mailer" (rails-core:mailer-file controller)))))
    (setq item
          (rails-core:menu
           (list (concat title " " controller
                         (when action (format " (%s)" action)))
                 (cons "Please select.."
                       item))))
    (typecase item
      (symbol (rails-controller-layout:switch-to item))
      (string (rails-core:find-file-if-exist item)))))

(provide 'rails-controller-layout)
