;;; rails.el --- minor mode for editing RubyOnRails code

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails.el $
;; $Id: rails.el 60 2007-01-13 20:01:21Z dimaexe $

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

(eval-when-compile
  (require 'speedbar)
  (require 'inf-ruby)
  (require 'ruby-mode))

(require 'ansi-color)
(require 'snippet)
(require 'etags)
(require 'find-recursive)
(require 'autorevert)

(require 'rails-core)
(require 'rails-lib)
(require 'rails-webrick)
(require 'rails-navigation)
(require 'rails-scripts)
(require 'rails-ui)

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

(defvar rails-version "0.4")
(defvar rails-templates-list '("rhtml" "rxml" "rjs"))
(defvar rails-use-another-define-key nil)
(defvar rails-primary-switch-func nil)
(defvar rails-secondary-switch-func nil)

(defvar rails-for-alist
  '(("rb" rails-for-helper (lambda (root) (string-match (concat root "app/helpers") buffer-file-name)))
    ("rb" rails-for-controller (lambda (root) (string-match (concat root "app/controllers") buffer-file-name)))
    ("rhtml" rails-for-layout (lambda (root) (string-match (concat root "app/views/layouts") buffer-file-name)))
    ("rhtml" rails-for-rhtml)))

(defvar rails-enviroments '("development" "production" "test"))

(defvar rails-adapters-alist
  '(("mysql"      . sql-mysql)
    ("postgresql" . sql-postgres)
    ("sqlite3" . sql-sqlite))
  "Sets emacs sql function for rails adapter names.")

(defvar rails-tags-dirs '("app" "lib" "test" "db")
  "List of directories from RAILS_ROOT where ctags works.")

(defun rails-use-text-menu ()
  "If t use text menu, popup menu otherwise"
  (or (null window-system) rails-always-use-text-menus))

(defvar rails-find-file-function 'find-file
  "Function witch called by rails finds")

;;;;;;;; hack ;;;;

;; replace in autorevert.el
(defun auto-revert-tail-handler ()
  (let ((size (nth 7 (file-attributes buffer-file-name)))
        (modified (buffer-modified-p))
        buffer-read-only    ; ignore
        (file buffer-file-name)
        buffer-file-name)   ; ignore that file has changed
    (when (> size auto-revert-tail-pos)
      (undo-boundary)
      (save-restriction
        (widen)
        (save-excursion
          (let ((cur-point (point-max)))
            (goto-char (point-max))
            (insert-file-contents file nil auto-revert-tail-pos size)
            (ansi-color-apply-on-region cur-point (point-max)))))
      (undo-boundary)
      (setq auto-revert-tail-pos size)
      (set-buffer-modified-p modified)))
  (set-visited-file-modtime))

(defun rails-svn-status-into-root ()
  (interactive)
  (rails-core:with-root (root)
                        (svn-status root)))

;; helper functions/macros
(defun rails-open-log (env)
  "Open Rails log file for environment ENV (development, production, test)"
  (interactive (list (rails-read-enviroment-name)))
  (rails-core:with-root
   (root)
   (let ((log-file (rails-core:file (concat "/log/" env ".log"))))
     (when (file-exists-p log-file)
         (find-file log-file)
         (set-buffer-file-coding-system 'utf-8)
         (ansi-color-apply-on-region (point-min) (point-max))
         (set-buffer-modified-p nil)
         (rails-minor-mode t)
         (goto-char (point-max))
         (setq auto-revert-interval 0.5)
         (auto-revert-set-timer)
         (setq auto-window-vscroll t)
         (make-local-variable 'rails-api-root)
         (auto-revert-tail-mode t)))))

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
  (rails-core:in-root
   (message "Creating TAGS, please wait...")
   (let ((tags-file-name (rails-core:file "TAGS")))
     (shell-command
      (format rails-tags-command tags-file-name
        (strings-join " " (mapcar #'rails-core:file rails-tags-dirs))))
     (flet ((yes-or-no-p (p) (if rails-ask-when-reload-tags
         (y-or-n-p p)
             t)))
       (visit-tags-table tags-file-name)))))

(defun rails-run-for-alist(root)
  (let ((ret nil)
        (alist rails-for-alist))
    (while (car alist)
      (let* ((it (car alist))
             (ext (concat "\\." (nth 0 it) "$"))
             (for-func (nth 1 it))
             (for-lambda (nth 2 it)))
        (if (string-match ext buffer-file-name)
            (progn
              (if (and for-lambda
                       (apply for-lambda (list root)))
                  (progn
                    (setq alist nil)
                    (require for-func)
                    (apply for-func nil)
                    (setq ret t)))
              (unless for-lambda
                (progn
                  (setq alist nil)
                  (require for-func)
                  (apply for-func nil)
                  (setq ret t))))))
      (setq alist (cdr alist)))
    ret))

;;;;;;;;;; Database integration ;;;;;;;;;;

(defstruct rails-db-conf adapter host database username password)

(defun rails-db-parameters (env)
  "Return database parameters for enviroment ENV"
  (with-temp-buffer
    (shell-command
     (format "ruby -r yaml -e 'YAML.load_file(%s)[\"%s\"].to_yaml.display'"
             (rails-core:quoted-file "config/database.yml")
             env)
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
  (require 'sql)
  (rails-core:with-root (root)
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
  (rails-core:with-root
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
  (make-local-variable 'rails-secondary-switch-func))

(add-hook 'ruby-mode-hook
          (lambda()
            (require 'rails-ruby)
            (syntax-table)
            (capitalize "AA/addd_aaa")
            (modify-syntax-entry ?! "w" (syntax-table))
            (modify-syntax-entry ?: "w" (syntax-table))
            (modify-syntax-entry ?_ "w" (syntax-table))
            (local-set-key (kbd "C-.") 'complete-tag)
            (local-set-key (if rails-use-another-define-key
                               (kbd "TAB") (kbd "<tab>"))
                           'ruby-indent-command)
            (local-set-key (if rails-use-another-define-key
                               (kbd "RET") (kbd "<return>"))
                           'ruby-newline-and-indent)))

(add-hook 'speedbar-mode-hook
          (lambda()
            (speedbar-add-supported-extension "\\.rb")))

(add-hook 'find-file-hooks
          (lambda()
            (rails-core:with-root
             (root)
             (progn
               (unless (string-match "[Mm]akefile" mode-name)
                 (add-hook 'local-write-file-hooks
                           '(lambda()
                              (when (eq this-command 'save-buffer)
                                (save-excursion
                                  (untabify (point-min) (point-max))
                                  (delete-trailing-whitespace))))))
               (rails-minor-mode t)
               (rails-run-for-alist root)
               (local-set-key (if rails-use-another-define-key "TAB" (kbd "<tab>"))
                              '(lambda() (interactive)
                                 (if snippet
                                     (snippet-next-field)
                                   (if (looking-at "\\>")
                                       (hippie-expand nil)
                                     (indent-according-to-mode)))))))))

;;; Run rails-minor-mode in dired
(add-hook 'dired-mode-hook
          (lambda ()
            (if (rails-core:root)
                (rails-minor-mode t))))

(provide 'rails)