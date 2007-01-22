;;; rails-core.el --- core helper functions and macros for emacs-rails

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-core.el $
;; $Id: rails-core.el 61 2007-01-21 17:26:12Z dimaexe $

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

(defun rails-core:root ()
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

(defmacro* rails-core:with-root ((root) &body body)
  "If you use `rails-core:root' or functions related on it
several times in a block of code, you can optimize your code by
using this macro. Also, blocks of code will be executed only if
rails-root exist.
 (rails-core:with-root (root)
    (foo root)
    (bar (rails-core:file \"some/path\")))
 "
 `(let ((,root (rails-core:root)))
    (when ,root
      (flet ((rails-core:root () ,root))
        ,@body))))

(defmacro rails-core:in-root (&rest body)
  "Set the default directory to the Rails root directory while
BODY is executed."
  (let ((root (gensym)))
    `(rails-core:with-root
      (,root)
      (let ((default-dir ,root))
        ,@body))))

(defvar rails-core:class-dirs
  '("app/controllers" "app/views" "app/models" "app/helpers"
    "test/unit" "test/functional" "test/fixtures")
  "Directories with Rails classes")

(defun rails-core:class-by-file (filename)
  "Return the class associated with FILENAME.
   <rails-root>/(app/models|app/controllers|app/helpers|test/unit|test/functional)/foo/bar_baz
                --> Foo::BarBaz"
  (let* ((case-fold-search nil)
         (path (replace-regexp-in-string
                (format
                 "\\(.*\\(%s\\)/\\)?\\([^\.]+\\)\\(.*\\)?"
                 (strings-join "\\|" rails-core:class-dirs)) "\\3" filename))
         (path (replace-regexp-in-string "/" "  " path))
         (path (replace-regexp-in-string "_" " " path)))
    (replace-regexp-in-string
     " " ""
     (replace-regexp-in-string
      "  " "::"
      (capitalize path)))))

(defun rails-core:file-by-class (classname &optional do-not-append-ext)
  "Return the filename associated with CLASSNAME.
If the optional parameter DO-NOT-APPEND-EXT is set this function
will not append \".rb\" to result."
  (let* ((case-fold-search nil)
         (path (replace-regexp-in-string "::" "/" classname))
         (path (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" path))
         (path (replace-regexp-in-string "\\([a-z\\d]\\)\\([A-Z]\\)" "\\1_\\2" path)))
    (concat (downcase path)
            (unless do-not-append-ext ".rb"))))

(defmacro rails-core:add-to-rails-menubar (item &rest prefix)
  "Add ITEM to the local Rails menubar, where ITEM is (cons
\"Menu title\" 'function)"
  `(local-set-key [menu-bar file ,@prefix]  ,item))

;;;;;;;;;; Project ;;;;;;;;;;

(defun rails-core:project-name ()
  "Return the name of current Rails project."
  (replace-regexp-in-string "^.*/\\(.*\\)/$" "\\1"
          (directory-name (rails-core:root))))

;;;;;;;;;; Files ;;;;;;;;;;

(defun rails-core:file (file-name)
  "Return the full path for FILE-NAME in a Rails directory."
  (when-bind (root (rails-core:root))
       (concat root file-name)))

(defun rails-core:quoted-file (file-name)
  "Return the quoted full path for FILE-NAME in a Rails directory."
  (concat "\"" (rails-core:file file-name) "\""))

(defun rails-core:find-file (file-name)
  "Open the file named FILE_NAME in a Rails directory."
  (when-bind (file (rails-core:file file-name))
       (find-file file)))

(defun rails-core:find-file-if-exist (file-name)
  "Open the file named FILE-NAME in a Rails directory only if the file exists."
  (let ((file-name (rails-core:file file-name)))
    (when (file-exists-p file-name)
  (find-file file-name))))

(defun rails-core:find-or-ask-to-create (question file)
  "Open the file named FILE in a Rails directory if it exists. If
it does not exist, ask to create it using QUESTION as a prompt."
  (find-or-ask-to-create question (rails-core:file file)))

;; Funtions, that retrun Rails objects full pathes

(defun rails-core:model-file (model-name)
  "Return teh model file from the model name."
  (concat "app/models/" (rails-core:file-by-class model-name)))

(defun rails-core:controller-file (controller-name)
  "Return the path to the controller CONTROLLER-NAME."
  (concat "app/controllers/"
    (rails-core:file-by-class
     (rails-core:short-controller-name controller-name) t)
    (unless (string-equal controller-name "Application") "_controller")
    ".rb"))

(defun rails-core:layout-file (layout)
  "Return the path to the layout file named LAYOUT."
  (concat "app/views/layouts/" layout ".rhtml"))

(defun rails-core:js-file (js)
  "Return the path to the JavaScript file named JS."
  (concat "public/javascripts/" js ".js"))

(defun rails-core:partial-name (name)
  "Return the file name of partial NAME."
  (if (string-match "/" name)
      (concat "app/views/"
        (replace-regexp-in-string "\\([^/]*\\)$" "_\\1.rhtml" name))
    (concat (rails-core:views-dir (rails-core:current-controller))
      "_" name ".rhtml")))

(defun rails-core:view-name (name)
  "Return the file name of view NAME."
  (concat (rails-core:views-dir (rails-core:current-controller))
          name ".rhtml"))

(defun rails-core:helper-file (controller)
  "Return the helper file name for the controller named
CONTROLLER."
  (format "app/helpers/%s_helper.rb"
          (replace-regexp-in-string "_controller" ""
                                    (rails-core:file-by-class controller t))))

(defun rails-core:functional-test-file (controller)
  "Return the functional test file name for the controller named
CONTROLLER."
  (format "test/functional/%s_test.rb"
    (rails-core:file-by-class (rails-core:long-controller-name controller) t)))

(defun rails-core:unit-test-file (model)
  "Return the unit test file name for the model named MODEL."
  (format "test/unit/%s_test.rb" (rails-core:file-by-class model t)))

(defun rails-core:fixtures-file (model)
  "Return the fixtures file name for the model named MODEL."
  ;;; Buggy: plurality conversion does not right
  (format "test/fixtures/%ss.yml" (rails-core:file-by-class model t)))

(defun rails-core:views-dir (controller)
  "Return the view directory name for the controller named CONTROLLER."
  (format "app/views/%s/" (rails-core:file-by-class controller t)))

(defun rails-core:stylesheet-name (name)
  "Return the file name of the stylesheet named NAME."
  (concat "public/stylesheets/" name ".css"))

(defun rails-core:full-controller-name (controller)
  "Return the class name of the controller named CONTROLLER.
   Bar in Foo dir -> Foo::Bar"
  (rails-core:class-by-file
   (if (eq (elt controller 0) 47) ;;; 47 == '/'
       (subseq controller 1)
     (let ((current-controller (rails-core:current-controller)))
       (if (string-match ":" current-controller)
     (concat (replace-regexp-in-string "[^:]*$" "" current-controller)
       controller)
   controller)))))

;;;;;;;;;; Functions that return collection of Rails objects  ;;;;;;;;;;

(defun rails-core:controllers (&optional cut-contoller-suffix)
  "Return a list of Rails controllers. Remove the '_controller'
suffix if CUT-CONTOLLER-SUFFIX is non nil."
  (mapcar
   (lambda (controller)
     (rails-core:class-by-file
      (if cut-contoller-suffix
    (replace-regexp-in-string "_controller\." "\." controller)
  controller)))
   (find-recursive-files "_controller\\.rb$" (rails-core:file "app/controllers/"))))

(defun rails-core:models ()
  "Return a list of Rails models."
  (mapcar
   #'rails-core:class-by-file
   (find-recursive-files "\\.rb$" (rails-core:file "app/models/"))))

(defun rails-core:regex-for-match-view ()
  "Return a regex to match Rails view templates.
The file extensions used for views are defined in
`rails-templates-list'."
  (let ((reg-string "\\.\\("))
    (mapcar (lambda (it) (setq reg-string (concat reg-string it "\\|"))) rails-templates-list)
    (concat (substring reg-string 0 -1) ")$")))

(defun rails-core:get-view-files (controller-class &optional action)
  "Retun a list containing the view file for CONTROLLER-CLASS#ACTION.
If the action is nil, return all views for the controller."
    (rails-core:with-root
     (root)
     (directory-files
      (rails-core:file
       (rails-core:views-dir
        (rails-core:short-controller-name controller-class))) t
        (if action
            (concat "^" action (rails-core:regex-for-match-view))
          (rails-core:regex-for-match-view)))))

(defun rails-core:extract-ancestors (classes)
  "Return the parent classes from a list of classes named CLASSES."
  (delete ""
   (uniq-list
   (mapcar (lambda (class)
       (replace-regexp-in-string
        "::[^:]*$" "::"
        (replace-regexp-in-string "^[^:]*$" "" class)))
     classes))))

(defun rails-core:models-ancestors ()
  "Return the parent classes of models."
  (rails-core:extract-ancestors (rails-core:models)))

(defun rails-core:controllers-ancestors ()
  "Return the parent classes of controllers."
  (rails-core:extract-ancestors (rails-core:controllers)))

;;;;;;;;;; Getting Controllers/Model/Action from current buffer ;;;;;;;;;;

(defun rails-core:current-controller ()
  "Return the current Rails controller."
  (let ((file-class (rails-core:class-by-file (buffer-file-name))))
    (case (rails-core:buffer-type)
      (:controller (rails-core:short-controller-name file-class))
      (:view (rails-core:class-by-file
              (directory-file-name (directory-of-file (buffer-file-name)))))
      (:helper (remove-postfix file-class "Helper"))
      (:functional-test (remove-postfix file-class "ControllerTest")))))

(defun rails-core:current-model ()
  "Return the current Rails model."
  (let ((file-class (rails-core:class-by-file (buffer-file-name))))
    (case (rails-core:buffer-type)
      (:model file-class)
      (:unit-test (remove-postfix file-class "Test"))
      ;;BUG!
      (:fixtures file-class))))

(defun rails-core:current-action ()
  "Return the current action in the current Rails controller."
  (case (rails-core:buffer-type)
    (:controller (save-excursion
       (when (search-backward-regexp "^[ ]*def \\([a-z0-9_]+\\)" nil t)
         (match-string 1))))
    (:view (string-match "/\\([a-z0-9_]+\\)\.[a-z]+$" (buffer-file-name))
     (match-string 1 (buffer-file-name)))))

(defun rails-core:current-helper ()
  "Return the current helper"
  (concat (rails-core:current-controller) "Helper"))

;;;;;;;;;; Determination of buffer type ;;;;;;;;;;

(defun rails-core:buffer-file-match (regexp)
  "Match the current buffer file name to RAILS_ROOT + REGEXP."
  (string-match (rails-core:file regexp)
    (buffer-file-name (current-buffer))))

(defun rails-core:buffer-type ()
  "Return the type of the current Rails file or nil if the type
cannot be determinated."
  (loop for (type dir) in rails-directory<-->types
        when (rails-core:buffer-file-match dir)
        do (return type)))

;;;;;;;;;; Openning of controller + action in controller and view ;;;;;;;;;;

(defun rails-core:open-controller+action-view (controller action)
  "Open the ACTION file for CONTROLLER in the views directory."
  (let ((controller (rails-core:file-by-class
                     (rails-core:short-controller-name controller) t)))
    (if action
        (let ((views (rails-core:get-view-files controller action)))
          (cond
           ((= (length views) 1) (find-file (first views)))
           ((= (length views) 0)
            (rails-core:find-or-ask-to-create
             (format "View for %s#%s does not exist, create it?" controller action)
             (format "app/views/%s/%s.rhtml" controller action)))
           (t (find-file
               (rails-core:menu
                (list "Please select view.."
                      (cons "Please select view.."
                            (loop for view in views collect
                                  (list
                                   (replace-regexp-in-string ".*\.r\\([A-Za-z]+\\)$" "\\1" view)
                                   view)))))))))
      (dired (rails-core:file (concat "app/views/" controller))))))

(defun rails-core:open-controller+action-controller (controller action)
  "Open CONTROLLER and go to ACTION."
  (if (rails-core:find-file-if-exist  (rails-core:controller-file controller))
      (progn
        (goto-char (point-min))
        (when action
          (if (search-forward-regexp (concat "^[ ]*def[ ]*" action) nil t)
              (recenter)))
        t)
    (error "Controller %s does not exist" controller)))

(defun rails-core:open-controller+action (where controller action)
  "Go to CONTROLLER/ACTION in WHERE."
  (ecase where
    (:view (rails-core:open-controller+action-view controller action))
    (:controller (rails-core:open-controller+action-controller controller action)))
  (message (concat controller (if action "#") action)))

;;;;;;;;;; Rails minor mode logs ;;;;;;;;;;

(defun rails-log-add (message)
  "Add MESSAGE to the Rails minor mode log in RAILS_ROOT."
  (rails-core:with-root
   (root)
   (append-string-to-file (rails-core:file "log/rails-minor-mode.log")
                          (format "%s: %s\n"
                                  (format-time-string "%Y/%m/%d %H:%M:%S") message))))

(defun rails-logged-shell-command (command buffer)
  "Execute a shell command in the buffer and write the results to
the Rails minor mode log."
  (shell-command (format "%s %s" rails-ruby-command command) buffer)
  (rails-log-add
   (format "\n%s> %s\n%s" (rails-core:project-name)
           command (buffer-string-by-name buffer))))

;;;;;;;;;; Rails menu ;;;;;;;;;;

(defun rails-core:menu-separator ()
  (unless (rails-use-text-menu) 'menu (list "--" "--")))

(defun rails-core:menu-of-views(controller &optional add-separator)
  "Make menu of view for CONTROLLER.
If optional parameter ADD_SEPARATOR is present, then add separator to menu."
  (let (menu)
    (setq menu
          (mapcar (lambda(i)
                    (list (concat (if (string-match "^_" (file-name-nondirectory i))
                                      "Partial" "View")
                                  ": "
                                  (file-name-nondirectory i))
                          i))
                  (rails-core:get-view-files controller nil)))
    (if (zerop (length menu))
        (setq menu (list))
      (if add-separator
          (add-to-list 'menu (rails-core:menu-separator))))
    menu))

(defun rails-core:menu (menu)
  "Show a menu."
  (let ((result
         (if (rails-use-text-menu)
             (tmm-prompt menu)
           (x-popup-menu (list '(200 100) (selected-window))
                         menu))))
    (if (listp result)
        (first result)
      result)))

;;;;;;;;;; Misc ;;;;;;;;;;

(defun rails-core:short-controller-name (controller)
  "Convert FooController -> Foo."
  (remove-postfix  controller "Controller" ))

(defun rails-core:long-controller-name (controller)
  "Convert Foo/FooController -> FooController."
  (if  (string-match "Controller$" controller)
      controller
    (concat controller "Controller")))

(defun rails-core:erb-block-string ()
  "Return the contents of the current ERb block."
  (save-excursion
    (save-match-data
      (let ((start (point)))
        (search-backward-regexp "<%[=]?")
        (let ((from (match-end 0)))
          (search-forward "%>")
          (let ((to (match-beginning 0)))
            (when (>= to start)
              (buffer-substring-no-properties from to))))))))

(defun rails-core:rhtml-buffer-p ()
  "Return non nil if the current buffer is rhtml file."
  (string-match "\\.rhtml$" (buffer-file-name)))

(provide 'rails-core)
