;;; rails-core.el --- core helper functions and macros for emacs-rails

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-core.el $
;; $Id: rails-core.el 51 2006-06-10 11:13:04Z crazypit $

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
  "Return RAILS_ROOT if this file is a part of rails
   application, else return nil"
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
 "If you use ``rails-core:root'' or functions ralated on it
  several times in block of code you can optimize you code by using
  this macro. Also block of code will be executed only if rails-root exist.
 (rails-core:with-root (root)
    (foo root)
    (bar (rails-core:file \"some/path\")))
 "
 `(let ((,root (rails-core:root)))
    (when ,root
      (flet ((rails-core:root () ,root))
        ,@body))))

(defmacro rails-core:in-root (&rest body)
  "Set default dir to Rails root dir while BODY executed"
  (let ((root (gensym)))
   `(rails-core:with-root (,root)
    (let ((default-dir ,root))
     ,@body))))

(defvar rails-core:class-dirs
  '("app/controllers" "app/views" "app/models" "app/helpers"
    "test/unit" "test/functional" "test/fixtures")
  "Dirs with Rails classes")

(defun rails-core:class-by-file (filename)
  "Return Class associated for FILENAME
   <rails-root>/(app/models|app/controllers|app/helpers|test/unit|test/functional)/foo/bar_baz
                --> Foo::BarBaz"
  (let* ((case-fold-search nil)
         (path (capitalize (replace-regexp-in-string
          (format
           "\\(.*\\(%s\\)/\\)?\\([^\.]+\\)\\(.*\\)?"
           (strings-join "\\|" rails-core:class-dirs)) "\\3" filename)))
         (path (replace-regexp-in-string "/" "::" path)))
    (replace-regexp-in-string "_" "" path)))

(defun rails-core:file-by-class (classname &optional do-not-append-ext)
  "Return filename associated for CLASSNAME,
   if optional parameter DO-NOT-APPEND-EXT is set
   this function not append \".rb\" to result"
  (let* ((case-fold-search nil)
         (path (replace-regexp-in-string "::" "/" classname))
         (path (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1_\\2" path))
         (path (replace-regexp-in-string "\\([a-z\\d]\\)\\([A-Z]\\)" "\\1_\\2" path)))
    (concat (downcase path)
            (unless do-not-append-ext ".rb"))))

;; (defun rails-core:get-controller-file (controller-class)
;;   "Return file contains controller CONTROLLER-CLASS"
;;   ;; Does not used
;;   (let ((file (rails-core:file-by-class controller-class))
;;         (root (rails-core:root)))
;;     (if (file-exists-p (concat root "app/controllers/" file))
;;         (concat root "app/controllers/" file))))

;; (defun rails-core:get-model-file (model-class)
;;   "Return file contains model MODEL-CLASS"
;;   ;; Does not used
;;   (let ((file (rails-core:file-by-class model-class))
;;         (root (rails-core:root)))
;;     (if (file-exists-p (concat root "app/models/" file))
;;         (concat root "app/models/" file))))

(defun rails-core:get-view-files (controller-class &optional action)
  "Retrun list contains views for CONTROLLER-CLASS#ACTON,
   if action nil, return all views for this controller"
    (rails-core:with-root
     (root)
     (directory-files
      (rails-core:file
       (rails-core:views-dir
        (rails-core:short-controller-name controller-class))) t
        (if action
            (concat "^" action (rails-core:regex-for-match-view))
          (rails-core:regex-for-match-view)))))

(defun rails-core:regex-for-match-view ()
  "Return regex to match rails view templates.
   File extension for view located in rails-templates-list"
  (let ((reg-string "\\.\\("))
    (mapcar (lambda (it) (setq reg-string (concat reg-string it "\\|"))) rails-templates-list)
    (concat (substring reg-string 0 -1) ")$")))

(defmacro rails-core:add-to-rails-menubar (item &rest prefix)
  "Add to ITEM local rails menubar
   ITEM is (cons \"Menu title\" 'func)"
  `(local-set-key [menu-bar file ,@prefix]  ,item))

;;;;;;;;;; Project ;;;;;;;;;;

(defun rails-core:project-name ()
  "Return name of current rails project"
  (replace-regexp-in-string "^.*/\\(.*\\)/$" "\\1"
          (directory-name (rails-core:root))))

;;;;;;;;;; Files ;;;;;;;;;;

(defun rails-core:file (file-name)
  "Return full path for ``file-name'' in rails-root"
  (when-bind (root (rails-core:root))
       (concat root file-name)))

(defun rails-core:quoted-file (file-name)
  "Return quoted full path for ``file-name''"
  (concat "\"" (rails-core:file file-name) "\""))

(defun rails-core:find-file (file-name)
  "Open file ``file-name'' in rails-root"
  (when-bind (file (rails-core:file file-name))
       (find-file file)))

(defun rails-core:find-file-if-exist (file-name)
  "Open file ``file-name'' in rails-root if this file exists"
  (let ((file-name (rails-core:file file-name)))
    (when (file-exists-p file-name)
  (find-file file-name))))

(defun rails-core:find-or-ask-to-create (question file)
  "Open file in Rails root if exist.
   If not exist ask to create it."
  (find-or-ask-to-create question (rails-core:file file)))

;; Funtions, that retrun Rails objects full pathes


(defun rails-core:model-file (model-name)
  "Return model file by model name"
  (concat "app/models/" (rails-core:file-by-class model-name)))

(defun rails-core:controller-file (controller-name)
  "Return path to controller ``controller-name''"
  (concat "app/controllers/"
    (rails-core:file-by-class
     (rails-core:short-controller-name controller-name) t)
    "_controller.rb"))

(defun rails-core:layout-file (layout)
  "Return PATH to layout file from Rails root"
  (concat "app/views/layouts/" layout ".rhtml"))

(defun rails-core:js-file (js)
  "Return PATH to JS file from Rails root"
  (concat "public/javascripts/" js ".js"))

(defun rails-core:partial-name (name)
  "Return file name of partial NAME"
  (if (string-match "/" name)
      (concat "app/views/"
        (replace-regexp-in-string "\\([^/]*\\)$" "_\\1.rhtml" name))
    (concat (rails-core:views-dir (rails-core:current-controller))
      "_" name ".rhtml")))

(defun rails-core:helper-file (controller)
  "Return helper file name of CONTROLLER"
  (format "app/helpers/%s_helper.rb"
          (replace-regexp-in-string "_controller" ""
                                    (rails-core:file-by-class controller t))))

(defun rails-core:functional-test-file (controller)
  "Return functional test file name of CONTROLLER"
  (format "test/functional/%s_test.rb"
    (rails-core:file-by-class (rails-core:long-controller-name controller) t)))

(defun rails-core:unit-test-file (model)
  "Return unit test file name of MODEL"
  (format "test/unit/%s_test.rb" (rails-core:file-by-class model t)))

(defun rails-core:fixtures-file (model)
  "Return unit test file name of MODEL"
  ;;; Buggy: plurality conversion does not right
  (format "test/fixtures/%ss.yml" (rails-core:file-by-class model t)))

(defun rails-core:views-dir (controller)
  "Return view directory name of Controller"
  (format "app/views/%s/" (rails-core:file-by-class controller t)))

(defun rails-core:stylesheet-name (name)
  "Return file name of stylesheet NAME"
  (concat "public/stylesheets/" name ".css"))

(defun rails-core:full-controller-name (controller)
  "Return contoller classname for rel controller name.
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
  "Return list of Rails controllers, cut '_contoller' suffix if
   cut-contoller-suffix non nil"
  (mapcar
   (lambda (controller)
     (rails-core:class-by-file
      (if cut-contoller-suffix
    (replace-regexp-in-string "_controller\." "\." controller)
  controller)))
   (find-recursive-files "_controller\\.rb$" (rails-core:file "app/controllers/"))))

(defun rails-core:models ()
  "Return list of Rails models"
  (mapcar
   #'rails-core:class-by-file
   (find-recursive-files "\\.rb$" (rails-core:file "app/models/"))))

(defun rails-core:extract-ancestors (classes)
  "Return prent classes from list of classes"
  (delete ""
   (uniq-list
   (mapcar (lambda (class)
       (replace-regexp-in-string
        "::[^:]*$" "::"
        (replace-regexp-in-string "^[^:]*$" "" class)))
     classes))))

(defun rails-core:models-ancestors ()
  "Return parent classes of models"
  (rails-core:extract-ancestors (rails-core:models)))

(defun rails-core:controllers-ancestors ()
  "Return parent classes of controller"
  (rails-core:extract-ancestors (rails-core:controllers)))

;;;;;;;;;; Getting Controllers/Model/Action from current buffer ;;;;;;;;;;

(defun rails-core:current-controller ()
  "Return current Rails controller"
  (let ((file-class (rails-core:class-by-file (buffer-file-name))))
    (case (rails-core:buffer-type)
      (:controller (rails-core:short-controller-name file-class))
      (:view (rails-core:class-by-file
        (directory-file-name (directory-of-file (buffer-file-name)))))
      (:helper (remove-postfix file-class "Helper"))
      (:functional-test (remove-postfix file-class "ControllerTest")))))

(defun rails-core:current-model ()
  "Return current Rails model"
  (let ((file-class (rails-core:class-by-file (buffer-file-name))))
    (case (rails-core:buffer-type)
      (:model file-class)
      (:unit-test (remove-postfix file-class "Test"))
      ;;BUG!
      (:fixtures file-class))))

(defun rails-core:current-action ()
  (case (rails-core:buffer-type)
    (:controller (save-excursion
       (when (search-backward-regexp "^[ ]*def \\([a-z_]+\\)" nil t)
         (match-string 1))))
    (:view (string-match "/\\([a-z_]+\\)\.[a-z]+$" (buffer-file-name))
     (match-string 1 (buffer-file-name)))))

;;;;;;;;;; Determination of buffer type ;;;;;;;;;;

(defun rails-core:buffer-file-match (regexp)
  "Match current buffer file name with RAILS ROOT + REGEXP"
  (string-match (rails-core:file regexp)
    (buffer-file-name (current-buffer))))

(defvar rails-core:directroy<-->types
  '((:controller       "app/controllers/")
    (:layout           "app/layouts/")
    (:view             "app/views/")
    (:model            "app/models/")
    (:helper           "app/helpers/")
    (:unit-test        "test/unit/")
    (:functional-test  "test/functional/")
    (:fixtures         "test/fixtures/"))
  "Rails file types -- rails dirs map")

(defun rails-core:buffer-type ()
  "Return type of current rails file or nil if can't determinate"
  (loop for (type dir) in rails-core:directroy<-->types
	when (rails-core:buffer-file-match dir)
	do (return type)))

;;;;;;;;;; Openning of controller + action in controller and view ;;;;;;;;;;

(defun rails-core:open-controller+action-view (controller action)
  "Open ACTION file for CONTROLLER in views."
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
  "Open CONTROLLER and goto ACTION"
  (if (rails-core:find-file-if-exist  (rails-core:controller-file controller))
      (progn
  (goto-char (point-min))
  (when action
    (if (search-forward-regexp (concat "^[ ]*def[ ]*" action))
        (recenter)))
  t)
    (error "Controller %s does not exist" controller)))

(defun rails-core:open-controller+action (where controller action)
  "Go to CONTROLLER/ACTION in WHERE"
  (ecase where
    (:view (rails-core:open-controller+action-view controller action))
    (:controller (rails-core:open-controller+action-controller controller action)))
  (message (concat controller (if action "#") action)))

;;;;;;;;;; Rails minor mode logs ;;;;;;;;;;

(defun rails-log-add (message)
  "Add ``message'' to rails monor mode log in ``rails-root''"
  (rails-core:with-root
   (root)
   (append-string-to-file (rails-core:file "log/rails-minor-mode.log")
    (format "%s: %s\n"
      (format-time-string "%Y/%m/%d %H:%M:%S") message))))

(defun rails-logged-shell-command (command buffer)
  "Execute shell command in buffer and write results to
  rails minor mode log"
  (shell-command command buffer)
  (rails-log-add
   (format "\n%s> %s\n%s" (rails-core:project-name)
     command (buffer-string-by-name buffer))))

;;;;;;;;;; Rails menu ;;;;;;;;;;

(defun rails-core:menu (menu)
  "Show menu"
  (let ((result
   (if rails-use-text-menu
       (tmm-prompt menu)
     (x-popup-menu (list (if (functionp 'posn-at-point) ; mouse position at point
           (destructuring-bind (x . y)
               (nth 2 (posn-at-point)) (list x y))
         '(200 100))
             (selected-window)) menu))))
    (if (listp result)
        (first result)
      result)))

;;;;;;;;;; Misc ;;;;;;;;;;

(defun rails-core:short-controller-name (controller)
  "Convert FooController -> Foo"
  (remove-postfix  controller "Controller" ))

(defun rails-core:long-controller-name (controller)
  "Convert Foo/FooController -> FooController"
  (if  (string-match "Controller$" controller)
      controller
    (concat controller "Controller")))

(defun rails-core:erb-block-string ()
  "Return string of current ERb block"
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
  "Return non nil if current buffer is rhtml file"
  (string-match "\\.rhtml$" (buffer-file-name)))

(provide 'rails-core)
