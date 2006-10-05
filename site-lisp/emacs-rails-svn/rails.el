;;; rails.el --- minor mode for editing RubyOnRails code

;; Copyright (C) 2006 Galinsky Dmitry <dima dot exe at gmail dot com>

;; Authors: Galinsky Dmitry <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails.el $
;; $Id: rails.el 52 2006-10-02 20:39:06Z dimaexe $

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

(defvar rails-version "0.3")
(defvar rails-ruby-command "ruby")
(defvar rails-templates-list '("rhtml" "rxml" "rjs"))
(defvar rails-chm-file nil "Path to CHM file or nil")
(defvar rails-use-another-define-key nil )
(defvar rails-primary-switch-func nil)
(defvar rails-secondary-switch-func nil)
(defvar rails-tags-command "ctags -e -a --Ruby-kinds=-f -o %s -R %s"
  "Command, that used for generating TAGS in Rails root")
(defvar rails-api-root nil
  "Root of Rails API html documentaion")


(defvar rails-for-alist
  '(
    ("rb" rails-for-controller (lambda (root) (string-match (concat root "app/controllers") buffer-file-name)))
    ("rhtml" rails-for-layout (lambda (root) (string-match (concat root "app/views/layouts") buffer-file-name)))
    ("rhtml" rails-for-rhtml)
    ))

(defvar rails-enviroments '("development" "production" "test"))

(defvar rails-adapters-alist
  '(("mysql"      . sql-mysql)
    ("postgresql" . sql-postgres))
  "Sets emacs sql function for rails adapter names.")

(defvar rails-tags-dirs '("app" "lib" "test" "db")
  "List of directories from RAILS_ROOT where ctags works.")

(defvar rails-ask-when-reload-tags nil
  "When t, every reloading of TAGS table you must confirm it.")

(defvar rails-use-text-menu nil
  "If t use text menu, popup menu otherwise")

(defvar rails-find-file-function 'find-file
  "Function witch called by rails finds")

(defvar rails-layout-template
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
          \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\"
      xml:lang=\"ru\" lang=\"ru\">
  <head>
    <title></title>
    <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />
    <%= stylesheet_link_tag \"style\" %>
    <!--[if IE]>
    <%= stylesheet_link_tag \"ie\" %>
    <![endif]-->
    <%= javascript_include_tag \"defaults\" %>
  </head>

  <body>
  </body>
</html>"
  "Default html template for new rails layout")

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
  "Open Rails log file for environment ``env''
   (development, production, test)"
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
        (let ((buf (buffer-file-name)))
          (unless (string= buf "*ri*")
            (switch-to-buffer-other-window "*ri*"))
          (setq buffer-read-only nil)
          (kill-region (point-min) (point-max))
          (message (concat "Please wait..."))
          (call-process "ri" nil "*ri*" t item)
          (setq buffer-read-only t)
          (local-set-key [return] 'rails-search-doc)
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

(defstruct rails-db-conf adapter database username password)

(defun rails-db-parameters (env)
  "Return database parameters for enviroment ENV"
  (rails-core:with-root
   (root)
   (save-excursion
     (rails-core:find-file "config/database.yml")
     (goto-line 1)
     (search-forward-regexp (format "^%s:" env))
     (let ((ans
      (make-rails-db-conf
       :adapter  (yml-next-value "adapter")
       :database (yml-next-value "database")
       :username (yml-next-value "username")
       :password (yml-next-value "password"))))
       (kill-buffer (current-buffer))
       ans))))

(defun rails-database-emacs-func (adapter)
  "Return emacs function, that running sql buffer by rails adapter name"
  (cdr (assoc adapter rails-adapters-alist)))

(defun rails-read-enviroment-name (&optional default)
  "Read rails enviroment with autocomplete"
  (completing-read "Environment name: " (list->alist rails-enviroments) nil nil default))

(defun* rails-run-sql (&optional env)
  "Run SQL process for current rails project."
  (interactive (list (rails-read-enviroment-name "development")))
  (require 'sql)
  (if (bufferp (sql-find-sqli-buffer))
      (switch-to-buffer-other-window (sql-find-sqli-buffer))
    (let ((conf (rails-db-parameters env)))
      (let ((sql-server "localhost")
            (sql-user (rails-db-conf-username conf))
            (sql-database (rails-db-conf-database conf))
            (sql-password (rails-db-conf-password conf))
            (default-process-coding-system '(utf-8 . utf-8)))
        ;; Reload localy sql-get-login to avoid asking of confirmation of DB login parameters
        (flet ((sql-get-login (&rest pars) () t))
          (funcall (rails-database-emacs-func (rails-db-conf-adapter conf))))))))

(defun rails-browse-api ()
  "Browse Rails API on RAILS-API-ROOT"
  (interactive)
  (browse-url (concat rails-api-root "index.html")))

(defun rails-get-api-entries (name file sexp get-file-func)
  (save-current-buffer
    (save-match-data
      (find-file (concat rails-api-root "/" file))
      (let* ((result
              (loop for line in (split-string (buffer-string) "\n")
                    when (string-match (format sexp (regexp-quote name)) line)
                    collect (cons (match-string 2 line)
                                  (match-string 1 line)))))
        (kill-buffer (current-buffer))
        (when-bind (api-file (funcall get-file-func result))
                   (browse-url (concat "file://" rails-api-root "/" api-file)))))))


(defun rails-browse-api-class (class)
  "Browse documentation in Rails API for CLASS."
  (rails-get-api-entries
   class "fr_class_index.html" "<a href=\"\\(.*\\)\">%s<"
   (lambda (entries)
     (cond ((= 0 (length entries)) (progn (message "No API Rails doc for class %s." class) nil))
           ((= 1 (length entries)) (cdar entries))))))

(defun rails-browse-api-method (method)
  "Browse documentation in Rails API for METHOD."
  (rails-get-api-entries
   method "fr_method_index.html" "<a href=\"\\(.*\\)\">%s[ ]+(\\(.*\\))"
   (lambda (entries)
     (cond ((= 0 (length entries)) (progn (message "No API Rails doc for %s" method) nil))
           ((= 1 (length entries)) (cdar entries))
           (t (cdr (assoc (completing-read (format "Method %s from what class? " method) entries)
                          entries)))))))

(defun rails-browse-api-at-point ()
  "Open html documentaion on class or method at point.
Please set variable rails-api-root to path for your local(!) Rails API directory"
  (interactive)
  (if rails-api-root
      (let ((current-symbol (prog2
                                (modify-syntax-entry ?: "w")
                                (thing-at-point 'sexp)
                              (modify-syntax-entry ?: "."))))
  (if (capital-word-p current-symbol)
      (rails-browse-api-class current-symbol)
    (rails-browse-api-method current-symbol)))
    (message "Please configure variable rails-api-root.")))
;;;;


(define-minor-mode rails-minor-mode
  "RubyOnRails"
  nil
  " RoR"
  rails-minor-mode-map

  (abbrev-mode -1)
  (make-local-variable 'tags-file-name)
  (make-local-variable 'rails-primary-switch-func)
  (make-local-variable 'rails-secondary-switch-func)
  ;(setq tags-file-name (concat (rails-core:root) "TAGS"))
  )

(add-hook 'ruby-mode-hook
          (lambda()
            (require 'rails-ruby)
            (modify-syntax-entry ?! "w")
            (local-set-key (kbd "C-.") 'complete-tag)
            (local-set-key (if rails-use-another-define-key
                               (kbd "TAB") (kbd "<tab>"))
                           'ruby-indent-or-complete)
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
                              (save-excursion
                                (untabify (point-min) (point-max))
                                (delete-trailing-whitespace)))))
               (rails-minor-mode t)
               (rails-run-for-alist root)
               (local-set-key (if rails-use-another-define-key "TAB" (kbd "<tab>"))
                              '(lambda() (interactive)
                                 (if snippet
                                     (snippet-next-field)
                                   (if (looking-at "\\>")
                                       (hippie-expand nil)
                                     (indent-for-tab-command)))))))))
;;; Run rails-minor-mode in dired
(add-hook 'dired-mode-hook
          (lambda ()
            (if (rails-core:root)
                (rails-minor-mode t))))

(provide 'rails)