;;; rails-navigation.el --- emacs-rails navigation functions

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://crazypit@rubyforge.org/var/svn/emacs-rails/trunk/rails-core.el $
;; $Id: rails-navigation.el 23 2006-03-27 21:35:16Z crazypit $

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

(defun rails-nav:goto-file-with-menu (dir title &optional ext no-inflector append-to-menu)
  "Make a menu to choose files from and find-file it."
  (let* (file
         files
         (ext (if ext ext "rb"))
         (ext (concat "\\." ext "$"))
         (root (rails-core:root))
         (dir (concat root dir))
         (mouse-coord (if (functionp 'posn-at-point) ; mouse position at point
                          (nth 2 (posn-at-point))
                        (cons 200 100))))
    (setq files (find-recursive-directory-relative-files dir "" ext))
    (setq files (sort files 'string<))
    (setq files (reverse files))
    (setq files (mapcar
                 (lambda(f)
                   (list (if no-inflector
                             f
                           (rails-core:class-by-file f)) f))
                 files))
    (if append-to-menu
        (add-to-list 'files append-to-menu t))

    (if files
  (progn
    (setq file (rails-core:menu
          (list title (cons title files))))
    (if file
        (if (symbolp file)
      (apply file nil)
    (find-file (concat dir file)))))
      (message "No files found"))))

(defun rails-nav:goto-controllers ()
  "Go to controllers."
  (interactive)
  (rails-nav:goto-file-with-menu "app/controllers/" "Go to controller.."))

(defun rails-nav:goto-models ()
  "Go to models."
  (interactive)
  (rails-nav:goto-file-with-menu "app/models/" "Go to model.."))

(defun rails-nav:goto-helpers ()
  "Go to helpers."
  (interactive)
  (rails-nav:goto-file-with-menu "app/helpers/" "Go to helper.."))

(defun rails-nav:create-new-layout (&optional name)
  "Create a new layout."
  (let ((name (or name (read-string "Layout name? ")))
        (root (rails-core:root)))
    (rails-core:find-file (rails-core:layout-file name))
    (if (y-or-n-p "Insert initial template? ")
        (insert rails-layout-template))))

(defun rails-nav:goto-layouts ()
  "Go to layouts."
  (interactive)
  (let ((path "app/views/layouts/")
        item)
    (setq item (cons "Create new layout" 'rails-nav:create-new-layout))
    (rails-nav:goto-file-with-menu path "Go to layout.." "rhtml" t item)))

(defun rails-nav:goto-stylesheets ()
  "Go to stylesheets."
  (interactive)
  (rails-nav:goto-file-with-menu "public/stylesheets/" "Go to stylesheet.." "css" t))

(defun rails-nav:goto-javascripts ()
  "Go tto JavaScripts."
  (interactive)
  (rails-nav:goto-file-with-menu "public/javascripts/" "Go to stylesheet.." "js" t))

(defun rails-nav:goto-migrate ()
  "Go to migrations."
  (interactive)
  (rails-nav:goto-file-with-menu "db/migrate/" "Go to migrate.." "rb" t))

;;;;;;;;;; Goto file on current line ;;;;;;;;;;

(defmacro* def-goto-line (name (&rest conditions) &rest body)
  "Go to the file specified by the current line. Parses the
current line for a series of patterns."
  (let ((line (gensym))
        (field (gensym))
        (prefix (gensym)))
    `(progn
       (defun ,name (,line ,prefix)
         (block ,name
           ,@(loop for (sexpr . map) in conditions
                   collect
                   `(when (string-match ,sexpr ,line)
                      (let ,(loop for var-acc in map collect
                                  (if (listp var-acc)
                                      `(,(first var-acc) (match-string ,(second var-acc) ,line))
                                    var-acc))
                        (return-from ,name (progn ,@body))))))))))

(defun rails-goto-file-on-current-line (prefix)
  "Analyze a string (or ERb block) and open some file related with it.
For example, on a line with \"render :partial\" runing this
function will open the partial file.  The function works with
\"layout 'name'\", \"render/redirect-to [:action => 'name',]
[controller => 'n']\", stylesheet_link_tag and other common
patterns.

Rules for actions/controllers:
 If you are in a controller, the cursor will be placed on the controller action.
 If you in view, the view file related to the action will be opened.
 Use prefix before the command to change this navigation direction."
  (interactive "P")
  (rails-core:with-root
   (root)
   (save-match-data
     (unless
         (when-bind
          (line (save-excursion
                  (if (rails-core:rhtml-buffer-p)
                      (rails-core:erb-block-string)
                    (current-line-string))))
          (loop for func in rails-on-current-line-gotos
                until (when (funcall func line prefix) (return t))))
       (message "Can't switch to some file form this line.")))))

(defvar rails-on-current-line-gotos
  '(rails-line-->partial
    rails-line-->action
    rails-line-->controller+action
    rails-line-->layout
    rails-line-->stylesheet
    rails-line-->js)
  "Functions that will ne called to analyze the line when
rails-goto-file-on-current-line is run.")

(def-goto-line rails-line-->stylesheet (("[ ]*stylesheet_link_tag[ ][\"']\\([^\"']*\\)[\"']"
                                         (name 1)))
  (rails-core:find-or-ask-to-create
   (format "Stylesheet \"%s\" does not exist do you whant to create it? " name)
   (rails-core:stylesheet-name name)))

(def-goto-line rails-line-->partial (("\\([ ]*render\\|replace_html\\|insert_html\\).*:partial[ ]*=>[ ]*[\"']\\([^\"']*\\)[\"']"
                                      (name 2)))
  (rails-core:find-or-ask-to-create
   (format "Partial \"%s\" does not exist do you whant to create it? " name)
   (rails-core:partial-name name)))

(def-goto-line rails-line-->action (("\\([ ]*render\\|replace_html\\|insert_html\\).*:action[ ]*=>[ ]*[\"'\:]\\([^\"']*\\)"
                                     (name 2)))
  (rails-core:find-or-ask-to-create
   (format "View \"%s\" does not exist do you whant to create it? " name)
   (rails-core:view-name name)))

(def-goto-line rails-line-->layout (("^[ ]*layout[ ]*[\"']\\(.*\\)[\"']" (name 1)))
  (let ((file-name (rails-core:layout-file name)))
    (if (file-exists-p (rails-core:file file-name))
        (rails-core:find-file file-name)
      (rails-nav:create-new-layout name))))

(def-goto-line rails-line-->js (("^[ ]*javascript_include_tag[ ]*[\"']\\(.*\\)[\"']"
                                 (name  1)))
  (rails-core:find-or-ask-to-create
   (format "JavaScript file \"%s\" does not exist do you whant to create it? " name)
   (rails-core:js-file name)))

(defvar rails-line-to-controller/action-keywords
  '("render" "redirect_to" "link_to" "form_tag" "start_form_tag" "render_component"
    "form_remote_tag" "link_to_remote"))

(defun rails-line-->controller+action (line prefix)
  (when (loop for keyword in rails-line-to-controller/action-keywords
              when (string-match (format "^[ ]*%s " keyword) line) do (return t))
    (let (action controller)
      (when (string-match ":action[ ]*=>[ ]*[\"']\\([^\"']*\\)[\"']" line)
        (setf action (match-string 1 line)))
      (when (string-match ":controller[ ]*=>[ ]*[\"']\\([^\"']*\\)[\"']" line)
        (setf controller (match-string 1 line)))
      (rails-core:open-controller+action
       (if (rails-core:rhtml-buffer-p)
           (if prefix :controller :view)
         (if prefix :view :controller))
       (if controller
           (rails-core:full-controller-name controller)
         (rails-core:current-controller))
       action))))

;;;;;;;;;; Go to file from file ;;;;;;;;;;

;;; For Models

(defun rails-by-model-switch-to (what file-func)
  (let ((model (rails-core:current-model)))
    (rails-core:find-or-ask-to-create
     (format "%s for model %s does not exist, create it? " what model)
     (funcall file-func  model))))

(defun rails-by-model-switch-to-model ()
  (rails-by-model-switch-to "Model" 'rails-core:model-file))

;; Plural BUGS!!!
;; (defun rails-goto-fixtures-->model ()
;;   (rails-goto-model-->simple
;;    "Model" 'rails-core:current-model-from-fixtures
;;    'rails-core:model-file))

;; (defun  rails-goto-fixtures-->unit-test ()
;;   (rails-goto-model-->simple
;;    "Unit test" 'rails-core:current-model-from-fixtures
;;   'rails-core:unit-test-file))

(defun rails-by-model-switch-to-unit-test ()
  (rails-by-model-switch-to "Unit test" 'rails-core:unit-test-file))

(defun rails-by-model-switch-to-fixtures ()
  (rails-by-model-switch-to "Fixtures" 'rails-core:fixtures-file))

(defvar rails-goto-file-from-file-actions
  '((:controller
     (:invisible        rails-for-controller:switch-to-view2)
     rails-for-controller:views-for-current-action
     ("Helper"          rails-for-controller:switch-to-helper)
     ("Functional test" rails-for-controller:switch-to-functional-test))
    (:view
     ("Controller"      rails-for-rhtml:switch-to-controller-action)
     ("Helper"          rails-for-controller:switch-to-helper)
     ("Functional test" rails-for-controller:switch-to-functional-test))
    (:helper
     ("Controller"      rails-for-controller:switch-to-controller)
     ("View"            rails-for-controller:switch-to-views))
    (:functional-test
     ("Controller"      rails-for-controller:switch-to-controller))
;;; For Models
    (:model
     ("Unit test" rails-by-model-switch-to-unit-test)
     ("Fixtures"  rails-by-model-switch-to-fixtures))
    ;; Plural BUGS!!!
    ;;     (rails-core:fixtures-buffer-p
    ;;      (rails-goto-fixtures-->model "Model test")
    ;;      (rails-goto-fixtures-->unit-test "Unit test"))
    (:unit-test
     ("Model"      rails-by-model-switch-to-model)
     ("Fixtures"   rails-by-model-switch-to-fixtures))))

(defun rails-goto-file-from-file (show-menu)
  "Deteminate type of file and goto another file.
  With prefix show menu with variants."
  (interactive "P")
  (rails-core:with-root
   (root)
   (let ((variants (rest (find (rails-core:buffer-type)
                               rails-goto-file-from-file-actions
                               :key #'first))))
     (if variants
         (let ((variants
                (loop for variant in variants
                      when (symbolp variant)
                      append (funcall variant)
                      else collect variant)))
           (progn
             ;; Menu
             (if show-menu
                 (when-bind
                  (goto (rails-core:menu
                         (list "Go To: "
                               (cons "goto"
                                     (loop for (title func) in variants
                                           when (not (eq title :invisible))
                                           collect `(,title  ,func))))))
                  (funcall goto))
               ;;
               (funcall (second (first variants))))))
       (message "Can't go to some file from this file.")))))

(defun rails-goto-file-from-file-with-menu ()
  "Deteminate type of file and goto another file (choose type from menu)"
  (interactive)
  (rails-goto-file-from-file t))

;;;;;;;;;; Rails finds ;;;;;;;;;;

(defun rails-find (path)
  "Open find-file in minbuffer for ``path'' in rails-root"
  (let ((default-directory (rails-core:file path)))
    (call-interactively rails-find-file-function)))

(defmacro* def-rails-find (name dir)
  "Define new rails-find function"
  `(defun ,name ()
     ,(format "Run find-file in Rails \"%s\" dir" dir)
     (interactive)
     (rails-find ,dir)))

(def-rails-find rails-find-controller "app/controllers/")

(def-rails-find rails-find-view "app/views/")

(def-rails-find rails-find-layout "app/views/layouts/")

(def-rails-find rails-find-db "db/")

(def-rails-find rails-find-public "public/")

(def-rails-find rails-find-helpers "app/helpers/")

(def-rails-find rails-find-models "app/models/")

(def-rails-find rails-find-config "config/")

(def-rails-find rails-find-stylesheets "public/stylesheets/")

(def-rails-find rails-find-javascripts "public/javascripts/")

(def-rails-find rails-find-migrate "db/migrate/")

(def-rails-find rails-find-fixtures "test/fixtures/")

(provide 'rails-navigation)
