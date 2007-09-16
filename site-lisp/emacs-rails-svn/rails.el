;;; rails.el --- minor mode for editing RubyOnRails code

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails.el $
;; $Id: rails.el 214 2007-09-11 14:37:48Z crazypit $

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

(unless (<= 22 emacs-major-version)
  (error
   (format "emacs-rails require CVS version of Emacs (future Emacs 22), and not be running on your Emacs %s.%s"
           emacs-major-version
           emacs-minor-version)))

(eval-when-compile
  (require 'speedbar)
  (require 'inf-ruby)
  (require 'ruby-mode)
  (require 'ruby-electric))

(require 'sql)
(require 'ansi-color)
(require 'etags)
(require 'find-recursive)

(require 'untabify-file)
(require 'predictive-prog-mode)

(require 'inflections)

(require 'rails-compat)
(require 'rails-project)

(require 'rails-core)
(require 'rails-ruby)
(require 'rails-lib)

(require 'rails-cmd-proxy)
(require 'rails-navigation)
(require 'rails-find)
(require 'rails-scripts)
(require 'rails-rake)
(require 'rails-test)
(require 'rails-ws)
(require 'rails-log)
(require 'rails-ui)
(require 'rails-model-layout)
(require 'rails-controller-layout)
(require 'rails-features)
(require 'rails-spec)


;;;;;;;;;; Variable definition ;;;;;;;;;;

(defgroup rails nil
  "Edit Rails projet with Emacs."
  :group 'programming
  :prefix "rails-")

(defcustom rails-api-root nil
  "*Root of Rails API html documentation. Must be a local directory."
  :group 'rails
  :type 'string)

(defcustom rails-use-alternative-browse-url nil
  "Indicates an alternative way of loading URLs on Windows.
Try using the normal method before. If URLs invoked by the
program don't end up in the right place, set this option to
true."
  :group 'rails
  :type 'boolean)

(defcustom rails-browse-api-with-w3m nil
  "Indicates that the user wants to browse the Rails API using
Emacs w3m browser."
  :group 'rails
  :type 'boolean)

(defcustom rails-tags-command "ctags -e -a --Ruby-kinds=-f -o %s -R %s"
  "Command used to generate TAGS in Rails root"
  :group 'rails
  :type 'string)

(defcustom rails-ri-command "ri"
  "Command used to invoke the ri utility."
  :group 'rails
  :type 'string)

(defcustom rails-always-use-text-menus nil
  "Force the use of text menus by default."
  :group 'rails
  :type 'boolean)

(defcustom rails-ask-when-reload-tags nil
  "Indicates whether the user should confirm reload a TAGS table or not."
  :group 'rails
  :type 'boolean)

(defcustom rails-chm-file nil
  "Path to CHM documentation file on Windows, or nil."
  :group 'rails
  :type 'string)

(defcustom rails-ruby-command "ruby"
  "Ruby preferred command line invocation."
  :group 'rails
  :type 'string)

(defcustom rails-layout-template
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\"
      xml:lang=\"en\" lang=\"en\">
  <head>
    <title></title>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
    <%= stylesheet_link_tag \"default\" %>
  </head>

  <body>
    <%= yield %>
  </body>
</html>"
  "Default html template for new rails layout"
  :group 'rails
  :type 'string)

(defvar rails-version "0.5.99.1")
(defvar rails-templates-list '("erb" "rhtml" "rxml" "rjs" "haml" "liquid" "mab"))
(defvar rails-use-another-define-key nil)
(defvar rails-primary-switch-func nil)
(defvar rails-secondary-switch-func nil)

(defvar rails-directory<-->types
  '((:controller       "app/controllers/")
    (:layout           "app/layouts/")
    (:view             "app/views/")
    (:observer         "app/models/" (lambda (file) (rails-core:observer-p file)))
    (:mailer           "app/models/" (lambda (file) (rails-core:mailer-p file)))
    (:model            "app/models/" (lambda (file) (and (not (rails-core:mailer-p file))
                                                         (not (rails-core:observer-p file)))))
    (:helper           "app/helpers/")
    (:plugin           "vendor/plugins/")
    (:unit-test        "test/unit/")
    (:functional-test  "test/functional/")
    (:fixture          "test/fixtures/")
    (:migration        "db/migrate"))
  "Rails file types -- rails directories map")

(defvar rails-enviroments '("development" "production" "test"))
(defvar rails-default-environment (first rails-enviroments))

(defvar rails-adapters-alist
  '(("mysql"      . sql-mysql)
    ("postgresql" . sql-postgres)
    ("sqlite3"    . sql-sqlite))
  "Sets emacs sql function for rails adapter names.")

(defvar rails-tags-dirs '("app" "lib" "test" "db")
  "List of directories from RAILS_ROOT where ctags works.")

(defun rails-use-text-menu ()
  "If t use text menu, popup menu otherwise"
  (or (null window-system) rails-always-use-text-menus))

;;;;;;;; hack ;;;;
(defun rails-svn-status-into-root ()
  (interactive)
  (rails-project:with-root (root)
                           (svn-status root)))

;; helper functions/macros
(defun rails-search-doc (&optional item)
  (interactive)
  (setq item (if item item (thing-at-point 'sexp)))
  (unless item
    (setq item (read-string "Search symbol: ")))
  (if item
      (if (and rails-chm-file
               (file-exists-p rails-chm-file))
          (start-process "keyhh" "*keyhh*" "keyhh.exe" "-#klink"
                         (format "'%s'" item)  rails-chm-file)
        (let ((buf (buffer-name)))
          (unless (string= buf "*ri*")
            (switch-to-buffer-other-window "*ri*"))
          (setq buffer-read-only nil)
          (kill-region (point-min) (point-max))
          (message (concat "Please wait..."))
          (call-process rails-ri-command nil "*ri*" t item)
          (local-set-key [return] 'rails-search-doc)
          (ansi-color-apply-on-region (point-min) (point-max))
          (setq buffer-read-only t)
          (goto-char (point-min))))))

(defun rails-create-tags()
  "Create tags file"
  (interactive)
  (rails-project:in-root
   (message "Creating TAGS, please wait...")
   (let ((tags-file-name (rails-core:file "TAGS")))
     (shell-command
      (format rails-tags-command tags-file-name
        (strings-join " " (mapcar #'rails-core:file rails-tags-dirs))))
     (flet ((yes-or-no-p (p) (if rails-ask-when-reload-tags
         (y-or-n-p p)
             t)))
       (visit-tags-table tags-file-name)))))

(defun rails-apply-for-buffer-type ()
 (let* ((type (rails-core:buffer-type))
        (name (substring (symbol-name type) 1))
        (minor-mode-name (format "rails-%s-minor-mode" name))
        (minor-mode-abbrev (concat minor-mode-name "-abbrev-table")))
   (when (require (intern minor-mode-name) nil t) ;; load new style minor mode rails-*-minor-mode
     (when (fboundp (intern minor-mode-name))
       (apply (intern minor-mode-name) (list t))
       (when (boundp (intern minor-mode-abbrev))
         (merge-abbrev-tables
          (symbol-value (intern minor-mode-abbrev))
          local-abbrev-table))))))

;;;;;;;;;; Database integration ;;;;;;;;;;

(defstruct rails-db-conf adapter host database username password)

(defun rails-db-parameters (env)
  "Return database parameters for enviroment ENV"
  (with-temp-buffer
    (shell-command
     (format "ruby -r yaml -r erb -e 'YAML.load(ERB.new(ARGF.read).result)[\"%s\"].to_yaml.display' %s"
             env
             (rails-core:file "config/database.yml"))
     (current-buffer))
    (let ((answer
           (make-rails-db-conf
            :adapter  (yml-value "adapter")
            :host     (yml-value "host")
            :database (yml-value "database")
            :username (yml-value "username")
            :password (yml-value "password"))))
      answer)))

(defun rails-database-emacs-func (adapter)
  "Return the Emacs function for ADAPTER that, when run, will
+invoke the appropriate database server console."
  (cdr (assoc adapter rails-adapters-alist)))

(defun rails-read-enviroment-name (&optional default)
  "Read Rails enviroment with auto-completion."
  (completing-read "Environment name: " (list->alist rails-enviroments) nil nil default))

(defun* rails-run-sql (&optional env)
  "Run a SQL process for the current Rails project."
  (interactive (list (rails-read-enviroment-name "development")))
  (rails-project:with-root (root)
    (cd root)
    (if (bufferp (sql-find-sqli-buffer))
        (switch-to-buffer-other-window (sql-find-sqli-buffer))
      (let ((conf (rails-db-parameters env)))
        (let ((sql-database (rails-db-conf-database conf))
              (default-process-coding-system '(utf-8 . utf-8))
              (sql-server (rails-db-conf-host conf))
              (sql-user (rails-db-conf-username conf))
              (sql-password (rails-db-conf-password conf)))
          ;; Reload localy sql-get-login to avoid asking of confirmation of DB login parameters
          (flet ((sql-get-login (&rest pars) () t))
            (funcall (rails-database-emacs-func (rails-db-conf-adapter conf)))))))))

(defun rails-has-api-root ()
  "Test whether `rails-api-root' is configured or not, and offer to configure
it in case it's still empty for the project."
  (rails-project:with-root
   (root)
   (unless (or (file-exists-p (rails-core:file "doc/api/index.html"))
         (not (yes-or-no-p (concat "This project has no API documentation. "
           "Would you like to configure it now? "))))
     (let (clobber-gems)
       (message "This may take a while. Please wait...")
       (unless (file-exists-p (rails-core:file "vendor/rails"))
   (setq clobber-gems t)
   (message "Freezing gems...")
   (shell-command-to-string "rake rails:freeze:gems"))
       ;; Hack to allow generation of the documentation for Rails 1.0 and 1.1
       ;; See http://dev.rubyonrails.org/ticket/4459
       (unless (file-exists-p (rails-core:file "vendor/rails/activesupport/README"))
   (write-string-to-file (rails-core:file "vendor/rails/activesupport/README")
             "Placeholder"))
       (message "Generating documentation...")
       (shell-command-to-string "rake doc:rails")
       (if clobber-gems
     (progn
       (message "Unfreezing gems...")
       (shell-command-to-string "rake rails:unfreeze")))
       (message "Done...")))
   (if (file-exists-p (rails-core:file "doc/api/index.html"))
       (setq rails-api-root (rails-core:file "doc/api")))))

(defun rails-browse-api ()
  "Browse Rails API on RAILS-API-ROOT."
  (interactive)
  (if (rails-has-api-root)
      (rails-browse-api-url (concat rails-api-root "/index.html"))
    (message "Please configure variable rails-api-root.")))

(defun rails-get-api-entries (name file sexp get-file-func)
  "Return all API entries named NAME in file FILE using SEXP to
find matches, and GET-FILE-FUNC to process the matches found."
  (if (file-exists-p (concat rails-api-root "/" file))
      (save-current-buffer
        (save-match-data
          (find-file (concat rails-api-root "/" file))
          (let* ((result
                  (loop for line in (split-string (buffer-string) "\n")
                        when (string-match (format sexp (regexp-quote name)) line)
                        collect (cons (match-string-no-properties 2 line)
                                      (match-string-no-properties 1 line)))))
            (kill-buffer (current-buffer))
            (when-bind (api-file (funcall get-file-func result))
                       (rails-browse-api-url (concat "file://" rails-api-root "/" api-file))))))
    (message "There are no API docs.")))

(defun rails-browse-api-class (class)
  "Browse the Rails API documentation for CLASS."
  (rails-get-api-entries
   class "fr_class_index.html" "<a href=\"\\(.*\\)\">%s<"
   (lambda (entries)
     (cond ((= 0 (length entries)) (progn (message "No API Rails doc for class %s." class) nil))
           ((= 1 (length entries)) (cdar entries))))))

(defun rails-browse-api-method (method)
  "Browse the Rails API documentation for METHOD."
  (rails-get-api-entries
   method "fr_method_index.html" "<a href=\"\\(.*\\)\">%s[ ]+(\\(.*\\))"
   (lambda (entries)
     (cond ((= 0 (length entries)) (progn (message "No API Rails doc for %s" method) nil))
           ((= 1 (length entries)) (cdar entries))
           (t (cdr (assoc (completing-read (format "Method %s from what class? " method) entries)
                          entries)))))))

(defun rails-browse-api-at-point ()
  "Open the Rails API documentation on the class or method at the current point.
The variable `rails-api-root' must be pointing to a local path
either in your project or elsewhere in the filesystem. The
function will also offer to build the documentation locally if
necessary."
  (interactive)
  (if (rails-has-api-root)
      (let ((current-symbol (prog2
                                (modify-syntax-entry ?: "w")
                                (thing-at-point 'sexp)
                              (modify-syntax-entry ?: "."))))
        (if current-symbol
            (if (capital-word-p current-symbol)
                (rails-browse-api-class current-symbol)
              (rails-browse-api-method current-symbol))))
    (message "Please configure \"rails-api-root\".")))

;;; Rails minor mode

(define-minor-mode rails-minor-mode
  "RubyOnRails"
  nil
  " RoR"
  rails-minor-mode-map
  (abbrev-mode -1)
  (make-local-variable 'tags-file-name)
  (make-local-variable 'rails-primary-switch-func)
  (make-local-variable 'rails-secondary-switch-func)
  (rails-features:install))

;; hooks

(add-hook 'ruby-mode-hook
          (lambda()
            (require 'rails-ruby)
            (require 'ruby-electric)
            (ruby-electric-mode t)
            (ruby-hs-minor-mode t)
            (imenu-add-to-menubar "IMENU")
            (modify-syntax-entry ?! "w" (syntax-table))
            (modify-syntax-entry ?: "w" (syntax-table))
            (modify-syntax-entry ?_ "w" (syntax-table))
            (local-set-key (kbd "C-.") 'complete-tag)
            (local-set-key (if rails-use-another-define-key
                               (kbd "TAB") (kbd "<tab>"))
                           'indent-and-complete)
            (local-set-key (rails-key "f") '(lambda()
                                              (interactive)
                                              (mouse-major-mode-menu (rails-core:menu-position))))
            (local-set-key (kbd "C-:") 'ruby-toggle-string<>simbol)
            (local-set-key (if rails-use-another-define-key
                               (kbd "RET") (kbd "<return>"))
                           'ruby-newline-and-indent)))

(add-hook 'speedbar-mode-hook
          (lambda()
            (speedbar-add-supported-extension "\\.rb")))

(add-hook 'find-file-hooks
          (lambda()
            (rails-project:with-root
             (root)
             (progn
               (local-set-key (if rails-use-another-define-key
                                  (kbd "TAB") (kbd "<tab>"))
                              'indent-and-complete)
               (rails-minor-mode t)
               (rails-apply-for-buffer-type)))))

;; Run rails-minor-mode in dired

(add-hook 'dired-mode-hook
          (lambda ()
            (if (rails-project:root)
                (rails-minor-mode t))))


(autoload 'haml-mode "haml-mode" "" t)

(setq auto-mode-alist  (cons '("\\.rb$"     . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.rake$"   . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.mab$"    . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("Rakefile$"  . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.haml$"   . haml-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.rjs$"    . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.rxml$"   . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.rhtml$"  . html-mode) auto-mode-alist))

(modify-coding-system-alist 'file "\\.rb$"     'utf-8)
(modify-coding-system-alist 'file "\\.rake$"   'utf-8)
(modify-coding-system-alist 'file "Rakefile$" 'utf-8)
(modify-coding-system-alist 'file (rails-core:regex-for-match-view) 'utf-8)

(provide 'rails)
