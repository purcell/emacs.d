;;; rails-navigation.el --- emacs-rails navigation functions

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-navigation.el $
;; $Id: rails-navigation.el 203 2007-08-04 20:31:07Z dimaexe $

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

(defun rails-nav:create-goto-menu (items title &optional append-to-menu)
  (when append-to-menu
    (dolist (l append-to-menu items)
      (add-to-list 'items l t)))
  (let ((selected
         (when items
           (rails-core:menu
            (list title (cons title items))))))
    (if selected selected (message "No files found"))))

(defun rails-nav:goto-file-with-menu (dir title &optional ext no-inflector append-to-menu)
  "Make a menu to choose files from and find-file it."
  (let* (file
         files
         (ext (if ext ext "rb"))
         (ext (concat "\\." ext "$"))
         (dir (rails-core:file dir)))
    (setq files (find-recursive-directory-relative-files dir "" ext))
    (setq files (sort files 'string<))
    (setq files (mapcar
                 #'(lambda(f)
                     (list
                      (if no-inflector f (rails-core:class-by-file f))
                      f))
                 files))
    (when-bind
     (selected (rails-nav:create-goto-menu files title append-to-menu))
     (if (symbolp selected)
         (apply selected (list))
       (rails-core:find-file-if-exist (concat dir selected))))))

(defun rails-nav:goto-file-with-menu-from-list (items title func &optional append-to-menu)
  (when-bind
   (selected (rails-nav:create-goto-menu (list->alist items) title append-to-menu))
   (when-bind
    (file (apply func (list selected)))
    (rails-core:find-file-if-exist file))))

(defun rails-nav:goto-controllers ()
  "Go to controllers."
  (interactive)
  (rails-nav:goto-file-with-menu-from-list
   (rails-core:controllers t)
   "Go to controller"
   'rails-core:controller-file))

(defun rails-nav:goto-models ()
  "Go to models."
  (interactive)
  (rails-nav:goto-file-with-menu-from-list
   (rails-core:models)
   "Go to model.."
   'rails-core:model-file))

(defun rails-nav:goto-functional-tests ()
  "Go to functional tests."
  (interactive)
  (rails-nav:goto-file-with-menu-from-list
   (rails-core:functional-tests)
   "Go to functional test."
   'rails-core:functional-test-file))

(defun rails-nav:goto-unit-tests ()
  "Go to functional tests."
  (interactive)
  (rails-nav:goto-file-with-menu-from-list
   (rails-core:unit-tests)
   "Go to unit test."
   'rails-core:unit-test-file))

(defun rails-nav:goto-observers ()
  "Go to observers."
  (interactive)
  (rails-nav:goto-file-with-menu-from-list
   (rails-core:observers)
   "Go to observer.."
   'rails-core:observer-file))

(defun rails-nav:goto-mailers ()
  "Go to mailers."
  (interactive)
  (rails-nav:goto-file-with-menu-from-list
   (rails-core:mailers)
   "Go to mailers.."
   'rails-core:mailer-file))

(defun rails-nav:goto-migrate ()
  "Go to migrations."
  (interactive)
  (rails-nav:goto-file-with-menu-from-list
   (rails-core:migrations)
   "Go to migrate.."
   'rails-core:migration-file))

(defun rails-nav:goto-helpers ()
  "Go to helpers."
  (interactive)
  (rails-nav:goto-file-with-menu-from-list
   (rails-core:helpers)
   "Go to helper.."
   'rails-core:helper-file))

(defun rails-nav:goto-plugins ()
  "Go to plugins."
  (interactive)
  (rails-nav:goto-file-with-menu-from-list
   (rails-core:plugins)
   "Go to plugin.."
   (lambda (plugin)
     (concat "vendor/plugins/" plugin "/init.rb"))))

(defun rails-nav:create-new-layout (&optional name)
  "Create a new layout."
  (let ((name (or name (read-string "Layout name? "))))
    (when name
      (rails-core:find-file (rails-core:layout-file name))
      (if (y-or-n-p "Insert initial template? ")
          (insert rails-layout-template)))))

(defun rails-nav:goto-layouts ()
  "Go to layouts."
  (interactive)
  (let ((items (list (rails-core:menu-separator)
                     (cons "Create new layout" 'rails-nav:create-new-layout))))
    (rails-nav:goto-file-with-menu-from-list
     (rails-core:layouts)
     "Go to layout.."
     (lambda (l)
       (if (stringp l)
           (rails-core:layout-file l)
         (apply l (list))))
     items)))

(defun rails-nav:goto-fixtures ()
  "Go to fixtures."
  (interactive)
  (rails-nav:goto-file-with-menu-from-list
   (rails-core:fixtures)
   "Go to fixture.."
   'rails-core:fixture-file))

(defun rails-nav:goto-stylesheets ()
  "Go to stylesheets."
  (interactive)
  (rails-nav:goto-file-with-menu "public/stylesheets/" "Go to stylesheet.." "css" t))

(defun rails-nav:goto-javascripts ()
  "Go to JavaScripts."
  (interactive)
  (rails-nav:goto-file-with-menu "public/javascripts/" "Go to stylesheet.." "js" t))

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
\"layout 'name'\", \"render/redirect-to [:action => 'name',] [controller => 'n']\",
stylesheet_link_tag and other common
patterns.

Rules for actions/controllers:
 If you are in a controller, the cursor will be placed on the controller action.
 If you in view, the view file related to the action will be opened.
 Use prefix before the command to change this navigation direction."
  (interactive "P")
  (rails-project:with-root
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
       (message "Can't switch to some file from this line.")))))

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
      (rails-controller-layout:switch-to-action-in-controller
       (if controller controller
         (rails-core:current-controller))
       action))))

(provide 'rails-navigation)
