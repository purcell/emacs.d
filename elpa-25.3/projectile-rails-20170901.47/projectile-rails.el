;;; projectile-rails.el --- Minor mode for Rails projects based on projectile-mode

;; Copyright (C) 2013 Adam Sokolnicki

;; Author:            Adam Sokolnicki <adam.sokolnicki@gmail.com>
;; URL:               https://github.com/asok/projectile-rails
;; Package-Version: 20170901.47
;; Version:           0.15.0
;; Keywords:          rails, projectile
;; Package-Requires:  ((emacs "24.3") (projectile "0.12.0") (inflections "1.1") (inf-ruby "2.2.6") (f "0.13.0") (rake "0.3.2"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; To start it for the rails projects:
;;
;;    (projectile-rails-global-mode)
;;
;;; Code:

(require 'cl)
(require 'projectile)
(require 'inf-ruby)
(require 'inflections)
(require 'f)
(require 'rake)
(require 'json)

(defgroup projectile-rails nil
  "Rails mode based on projectile"
  :prefix "projectile-rails-"
  :group 'projectile)

(defcustom projectile-rails-controller-keywords
  '("logger" "polymorphic_path" "polymorphic_url" "mail" "render" "attachments"
    "default" "helper" "helper_attr" "helper_method" "layout" "url_for"
    "serialize" "exempt_from_layout" "filter_parameter_logging" "hide_action"
    "cache_sweeper" "protect_from_forgery" "caches_page" "cache_page"
    "caches_action" "expire_page" "expire_action" "rescue_from" "params"
    "request" "response" "session" "flash" "head" "redirect_to" "redirect_back"
    "render_to_string" "respond_with" "before_filter" "append_before_filter"
    "before_action" "append_before_action"
    "prepend_before_filter" "after_filter" "append_after_filter"
    "prepend_after_filter" "around_filter" "append_around_filter"
    "prepend_around_filter" "skip_before_filter" "skip_after_filter" "skip_filter"
    "prepend_before_action" "after_action" "append_after_action"
    "prepend_after_action" "around_action" "append_around_action"
    "prepend_around_action" "skip_before_action" "skip_after_action" "skip_action")
  "List of keywords to highlight for controllers."
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-migration-keywords
  '("create_table" "change_table" "drop_table" "rename_table" "add_column"
    "rename_column" "change_column" "change_column_default" "change_column_null"
    "remove_column" "add_index" "remove_index" "rename_index" "execute"
    "add_timestamps" "remove_timestamps" "add_foreign_key" "remove_foreign_key"
    "add_reference" "remove_reference" "add_belongs_to" "remove_belongs_to"
    "transaction" "reversible" "revert" "announce")
  "List of keywords to highlight for migrations."
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-model-keywords
  '("default_scope" "named_scope" "scope" "serialize" "belongs_to" "has_one"
    "has_many" "has_and_belongs_to_many" "composed_of" "accepts_nested_attributes_for"
    "before_create" "before_destroy" "before_save" "before_update" "before_validation"
    "before_validation_on_create" "before_validation_on_update" "after_create"
    "after_destroy" "after_save" "after_update" "after_validation"
    "after_validation_on_create" "after_validation_on_update" "around_create"
    "around_destroy" "around_save" "around_update" "after_commit" "after_find"
    "after_initialize" "after_rollback" "after_touch" "attr_accessible"
    "attr_protected" "attr_readonly" "validates" "validate" "validate_on_create"
    "validate_on_update" "validates_acceptance_of" "validates_associated"
    "validates_confirmation_of" "validates_each" "validates_exclusion_of"
    "validates_format_of" "validates_inclusion_of" "validates_length_of"
    "validates_numericality_of" "validates_presence_of" "validates_size_of"
    "validates_existence_of" "validates_uniqueness_of" "validates_with"
    "enum" "after_create_commit" "after_update_commit" "after_destroy_commit")
  "List of keywords to highlight for models."
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-view-keywords
  '("action_name" "atom_feed" "audio_path" "audio_tag" "auto_discovery_link_tag"
    "button_tag" "button_to" "button_to_function" "cache" "capture" "cdata_section"
    "check_box" "check_box_tag" "collection_select" "concat" "content_for"
    "content_tag" "content_tag_for" "controller" "controller_name"
    "controller_path" "convert_to_model" "cookies" "csrf_meta_tag" "csrf_meta_tags"
    "current_cycle" "cycle" "date_select" "datetime_select" "debug"
    "distance_of_time_in_words" "distance_of_time_in_words_to_now" "div_for"
    "dom_class" "dom_id" "email_field" "email_field_tag" "escape_javascript"
    "escape_once" "excerpt" "favicon_link_tag" "field_set_tag" "fields_for"
    "file_field" "file_field_tag" "flash" "form_for" "form_tag"
    "grouped_collection_select" "grouped_options_for_select" "headers"
    "hidden_field" "hidden_field_tag" "highlight" "image_alt" "image_path"
    "image_submit_tag" "image_tag" "j" "javascript_cdata_section"
    "javascript_include_tag" "javascript_path" "javascript_tag" "l" "label"
    "label_tag" "link_to" "link_to_function" "link_to_if" "link_to_unless"
    "link_to_unless_current" "localize" "logger" "mail_to" "number_field"
    "number_field_tag" "number_to_currency" "number_to_human" "number_to_human_size"
    "number_to_percentage" "number_to_phone" "number_with_delimiter"
    "number_with_precision" "option_groups_from_collection_for_select"
    "options_for_select" "options_from_collection_for_select" "params"
    "password_field" "password_field_tag" "path_to_audio" "path_to_image"
    "path_to_javascript" "path_to_stylesheet" "path_to_video" "phone_field"
    "phone_field_tag" "pluralize" "provide" "radio_button" "radio_button_tag"
    "range_field" "range_field_tag" "raw" "render" "request"
    "request_forgery_protection_token" "reset_cycle" "response" "safe_concat"
    "safe_join" "sanitize" "sanitize_css" "search_field" "search_field_tag"
    "select" "select_date" "select_datetime" "select_day" "select_hour"
    "select_minute" "select_month" "select_second" "select_tag" "select_time"
    "select_year" "session" "simple_format" "strip_links" "strip_tags"
    "stylesheet_link_tag" "stylesheet_path" "submit_tag" "t" "tag" "telephone_field"
    "telephone_field_tag" "text_area" "text_area_tag" "text_field" "text_field_tag"
    "time_ago_in_words" "time_select" "time_tag" "time_zone_options_for_select"
    "time_zone_select" "translate" "truncate" "url_field" "url_field_tag"
    "url_for" "url_options" "video_path" "video_tag" "word_wrap")
  "List of keywords to highlight for views."
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-active-support-keywords
  '("alias_attribute" "with_options" "delegate")
  "List of keywords to highlight for all `projectile-rails-mode' buffers."
  :group 'projectile-rails
  :type '(repeat string))

(defvar projectile-rails-keyword-face 'projectile-rails-keyword-face
  "Face name to use for keywords.")

(defface projectile-rails-keyword-face '((t :inherit 'font-lock-keyword-face))
  "Face to be used for higlighting the rails keywords."
  :group 'projectile-rails)

(defcustom projectile-rails-views-re
  (concat "\\."
          (regexp-opt '("html" "erb" "haml" "slim"
                        "js" "coffee" "ts"
                        "css" "scss" "less"
                        "json" "builder" "jbuilder" "rabl")))
  "Regexp for filtering for view files."
  :group 'projectile-rails
  :type 'regexp)

(defcustom projectile-rails-javascript-re
  "\\.js\\(?:\\.\\(?:coffee\\|ts\\)\\)?\\'"
  "Regexp for filtering for Javascript/altJS files."
  :group 'projectile-rails
  :type 'regexp)

(defcustom projectile-rails-stylesheet-re
  "\\.css\\(?:\\.\\(?:scss\\|sass\\|less\\)\\)?\\'"
  "Regexp for filtering for stylesheet files."
  :group 'projectile-rails
  :type 'regexp)

(defcustom projectile-rails-errors-re
  "\\([0-9A-Za-z@_./\:-]+\\.rb\\):?\\([0-9]+\\)?"
  "The regex used to find errors with file paths."
  :group 'projectile-rails
  :type 'regexp)

(defcustom projectile-rails-generate-filepath-re
  "^\\s-+\\(?:create\\|exists\\|conflict\\|skip\\)\\s-+\\(.+\\)$"
  "The regex used to find file paths in `projectile-rails-generate-mode'."
  :group 'projectile-rails
  :type 'regexp)

(defcustom projectile-rails-javascript-dirs
  '("app/assets/javascripts/" "lib/assets/javascripts/" "public/javascripts/")
  "The list of directories to look for the javascript files in."
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-component-dir "app/javascript/packs/"
  "The directory to look for javascript component files in."
  :group 'projectile-rails
  :type 'string)

(defcustom projectile-rails-stylesheet-dirs
  '("app/assets/stylesheets/" "lib/assets/stylesheets/" "public/stylesheets/")
  "The list of directories to look for the stylesheet files in."
  :group 'projectile-rails
  :type '(repeat string))

(defcustom projectile-rails-expand-snippet t
  "If not nil newly created buffers will be pre-filled with class skeleton."
  :group 'projectile-rails
  :type 'boolean)

(defcustom projectile-rails-add-keywords t
  "If not nil the rails keywords will be font locked in the mode's bufffers."
  :group 'projectile-rails
  :type 'boolean)

(defcustom projectile-rails-keymap-prefix (kbd "C-c r")
  "Keymap prefix for `projectile-rails-mode'."
  :group 'projectile-rails
  :type 'string)

(defcustom projectile-rails-server-mode-ansi-colors t
  "If not nil `projectile-rails-server-mode' will apply the ansi colors in its buffer."
  :group 'projectile-rails
  :type 'boolean)

(defcustom projectile-rails-discover-bind "s-r"
  "The :bind option that will be passed `discover-add-context-menu' if available."
  :group 'projectile-rails
  :type 'string)

(defcustom projectile-rails-vanilla-command "bundle exec rails"
  "The command for rails."
  :group 'projectile-rails
  :type 'string)

(defcustom projectile-rails-spring-command "bundle exec spring"
  "The command for spring."
  :group 'projectile-rails
  :type 'string)

(defcustom projectile-rails-zeus-command "zeus"
  "The command for zeus."
  :group 'projectile-rails
  :type 'string)

(defcustom projectile-rails-root-file "Gemfile"
  "The file that is used to identify rails root."
  :group 'projectile-rails
  :type 'string)

(defcustom projectile-rails-verify-root-file "config/routes.rb"
  "The file that is used to verify rails root directory."
  :group 'projectile-rails
  :type 'string)

(defvar projectile-rails-extracted-region-snippet
  '(("erb"  . "<%%= render '%s' %%>")
    ("haml" . "= render '%s'")
    ("slim" . "= render '%s'"))
  "A template used to insert text after extracting a region.")

(defvar projectile-rails-server-buffer-name "*projectile-rails-server*")

(defvar projectile-rails-fixture-dirs
  '("test/fixtures/" "test/factories/" "test/fabricators/"
    "spec/fixtures/" "spec/factories/" "spec/fabricators/"))

(defvar-local projectile-rails-zeus-sock nil
  "The path to the Zeus socket file")

(defvar projectile-rails-generators
  '(("assets" (("app/assets/"
                "app/assets/\\(?:stylesheets\\|javascripts\\)/\\(.+?\\)\\..+$")))
    ("controller" (("app/controllers/" "app/controllers/\\(.+\\)_controller\\.rb$")))
    ("generator" (("lib/generator/" "lib/generators/\\(.+\\)$")))
    ("helper" (("app/helpers/" "app/helpers/\\(.+\\)_helper.rb$")))
    ("integration_test" (("test/integration/" "test/integration/\\(.+\\)_test\\.rb$")))
    ("job" (("app/jobs/" "app/jobs/\\(.+\\)_job\\.rb$")))
    ("mailer" (("app/mailers/" "app/mailers/\\(.+\\)\\.rb$")))
    ("migration" (("db/migrate/" "db/migrate/[0-9]+_\\(.+\\)\\.rb$")))
    ("model" (("app/models/" "app/models/\\(.+\\)\\.rb$")))
    ("resource" (("app/models/" "app/models/\\(.+\\)\\.rb$")))
    ("scaffold" (("app/models/" "app/models/\\(.+\\)\\.rb$")))
    ("task" (("lib/tasks/" "lib/tasks/\\(.+\\)\\.rake$")))))

(defmacro projectile-rails-with-preloader (&rest cases)
  "Checks for the presence of pre-loaders and returns corresponding value.

CASES is a plist with props being :spring, :zeus or :vanilla.
Each corresponds to a preloader (:vanilla means no preloader).
If a preloader is running the value for the given prop is returned."
  `(cond ((projectile-rails-spring-p)
          ,(plist-get cases :spring))
         ((projectile-rails-zeus-p)
          ,(plist-get cases :zeus))
         (t
          ,(plist-get cases :vanilla))))

(defmacro projectile-rails-with-root (body-form)
  "Run BODY-FORM within DEFAULT-DIRECTORY set to `projectile-rails-root'"
  `(let ((default-directory (projectile-rails-root)))
     ,body-form))

(defmacro projectile-rails-find-current-resource (dir re fallback)
  "RE will be the argument to `s-lex-format'.

The bound variables are \"singular\" and \"plural\".
Argument DIR is the directory to which the search should be narrowed."
  `(let* ((singular (projectile-rails-current-resource-name))
          (plural (pluralize-string singular))
          (abs-current-file (buffer-file-name (current-buffer)))
          (current-file (if abs-current-file
                            (file-relative-name abs-current-file
                                                (projectile-rails-root))))
          (choices (projectile-rails-choices
                    (list (list ,dir (s-lex-format ,re)))))
          (files (projectile-rails-hash-keys choices)))
     (if (eq files ())
         (funcall ,fallback)
       (projectile-rails-goto-file
        (if (= (length files) 1)
            (gethash (-first-item files) choices)
          (projectile-rails--choose-file-or-new choices files))))))

(defun projectile-rails--choose-file-or-new (choices files)
  (let* ((choice (projectile-completing-read "Which exactly: " files))
         (candidate (gethash choice choices)))
    (if (f-exists? (projectile-rails-expand-root candidate))
        candidate
      (concat (f-dirname (gethash (-first-item files) choices)) choice))))

(defun projectile-rails-spring-p ()
  "Returns t if spring is running."
  (let ((root (directory-file-name (projectile-rails-root))))
    (or
     ;; Older versions
     (file-exists-p (format "%s/tmp/spring/spring.pid" root))
     ;; 0.9.2+
     (file-exists-p (format "%s/spring/%s.pid" temporary-file-directory (md5 root)))
     ;; 1.2.0+
     (let* ((path (or (getenv "XDG_RUNTIME_DIR") temporary-file-directory))
            (ruby-version (shell-command-to-string "ruby -e 'print RUBY_VERSION'"))
            (application-id (md5 (concat ruby-version root))))
       (or
        (file-exists-p (format "%s/spring/%s.pid" path application-id))
        ;; 1.5.0+
        (file-exists-p (format "%s/spring-%s/%s.pid" path (user-real-uid) application-id)))))))

(defun projectile-rails-zeus-p ()
  "Returns t if zeus is running."
  (unless projectile-rails-zeus-sock
    (setq
     projectile-rails-zeus-sock
     (or (getenv "ZEUSSOCK") (projectile-rails-expand-root ".zeus.sock"))))
  (file-exists-p projectile-rails-zeus-sock))

(defun projectile-rails-highlight-keywords (keywords)
  "Highlight the passed KEYWORDS in current buffer."
  (font-lock-add-keywords
   nil
   (list (list
          (concat "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b"
                  (regexp-opt keywords t)
                  "\\_>")
          (list 2 'projectile-rails-keyword-face)))))

(defun projectile-rails-add-keywords-for-file-type ()
  "Apply extra font lock keywords specific to models, controllers etc."
  (loop for (re keywords) in `(("_controller\\.rb$"   ,projectile-rails-controller-keywords)
                               ("app/models/.+\\.rb$" ,projectile-rails-model-keywords)
                               ("db/migrate/.+\\.rb$" ,projectile-rails-migration-keywords))
        do (when (and (buffer-file-name) (string-match-p re (buffer-file-name)))
             (projectile-rails-highlight-keywords
              (append keywords projectile-rails-active-support-keywords)))))

(defun projectile-rails-dir-files (directory)
  "Like `projectile-dir-files' but take `projectile-rails-root'."
  (--map
   (substring it (length (projectile-rails-root-relative-to-project-root)))
   (projectile-dir-files directory)))

(defun projectile-rails-choices (dirs)
  "Uses `projectile-rails-dir-files' function to find files in directories.

The DIRS is list of lists consisting of a directory path and regexp to filter files from that directory.
Returns a hash table with keys being short names and values being relative paths to the files."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for (dir re) in dirs do
          (loop for file in (projectile-rails-dir-files (projectile-rails-expand-root dir)) do
                (when (string-match re file)
                  (puthash (match-string 1 file) file hash))))
    hash))

(defun projectile-rails-hash-keys (hash)
  (if (boundp 'hash-table-keys)
      (hash-table-keys hash)
    (let (keys)
      (maphash (lambda (key value) (setq keys (cons key keys))) hash)
      keys)))

(defmacro projectile-rails-find-resource (prompt dirs &optional newfile-template)
  "Presents files from DIRS to the user using `projectile-completing-read'.

If users chooses a non existant file and NEWFILE-TEMPLATE is not nil
it will use that variable to interpolate the name for the new file.
NEWFILE-TEMPLATE will be the argument for `s-lex-format'.
The bound variable is \"filename\"."
  `(let* ((choices (projectile-rails-choices ,dirs))
          (filename (or
                     (projectile-completing-read ,prompt (projectile-rails-hash-keys choices))
                     (user-error "The completion system you're using does not allow inputting arbitrary value.")))
          (filepath (gethash filename choices)))
     (if filepath
         (projectile-rails-goto-file filepath)
       (when ,newfile-template
         (projectile-rails-goto-file (s-lex-format ,newfile-template) t)))))

(defun projectile-rails-find-model ()
  "Find a model."
  (interactive)
  (projectile-rails-find-resource
   "model: "
   '(("app/models/" "/models/\\(.+\\)\\.rb$"))
   "app/models/${filename}.rb"))

(defun projectile-rails-find-controller ()
  "Find a controller."
  (interactive)
  (projectile-rails-find-resource
   "controller: "
   '(("app/controllers/" "/controllers/\\(.+?\\)\\(_controller\\)?\\.rb$"))
   "app/controllers/${filename}_controller.rb"))

(defun projectile-rails-find-serializer ()
  "Find a serializer."
  (interactive)
  (projectile-rails-find-resource
   "serializer: "
   '(("app/serializers/" "/serializers/\\(.+\\)_serializer\\.rb$"))
   "app/serializers/${filename}_serializer.rb"))

(defun projectile-rails-find-view ()
  "Find a template or a partial."
  (interactive)
  (projectile-rails-find-resource
   "view: "
   `(("app/views/" ,(concat "app/views/\\(.+\\)" projectile-rails-views-re)))
   "app/views/${filename}"))

(defun projectile-rails-find-layout ()
  "Find a layout file."
  (interactive)
  (projectile-rails-find-resource
   "layout: "
   `(("app/views/layouts/" ,(concat "app/views/layouts/\\(.+\\)" projectile-rails-views-re)))
   "app/views/layouts/${filename}"))

(defun projectile-rails-find-rake-task (arg)
  "Find a file where a task is defined."
  (interactive "P")
  (rake-find-task arg))

(defun projectile-rails-find-helper ()
  "Find a helper."
  (interactive)
  (projectile-rails-find-resource
   "helper: "
   '(("app/helpers/" "/helpers/\\(.+\\)_helper\\.rb$"))
   "app/helpers/${filename}_helper.rb"))

(defun projectile-rails-find-lib ()
  "Find a file within lib directory."
  (interactive)
  (projectile-rails-find-resource
   "lib: "
   '(("lib/" "lib/\\(.+\\)\\.rb$"))
   "lib/${filename}.rb"))

(defun projectile-rails-find-spec ()
  "Find a spec."
  (interactive)
  (projectile-rails-find-resource
   "spec: "
   '(("spec/" "spec/\\(.+\\)_spec\\.rb$"))
   "spec/${filename}_spec.rb"))

(defun projectile-rails-find-test ()
  "Find a test."
  (interactive)
  (projectile-rails-find-resource
   "test: "
   '(("test/" "test/\\(.+\\)_test\\.rb$"))
   "test/${filename}_test.rb"))

(defun projectile-rails-find-fixture ()
  "Find a fixture file."
  (interactive)
  (projectile-rails-find-resource
   "fixture: "
   (--map (list it (concat it "\\(.+?\\)\\(?:_fabricator\\)?\\.\\(?:rb\\|yml\\)$"))
          projectile-rails-fixture-dirs)))

(defun projectile-rails-find-feature ()
  "Find a feature file."
  (interactive)
  (projectile-rails-find-resource
   "feature: "
   '(("features/" "features/\\(.+\\)\\.feature$"))
   "features/${filename}.feature"))

(defun projectile-rails-find-migration ()
  "Find a migration."
  (interactive)
  (projectile-rails-find-resource "migration: " '(("db/migrate/" "db/migrate/\\(.+\\)\\.rb$"))))

(defun projectile-rails-find-javascript ()
  "Find a javascript file."
  (interactive)
  (projectile-rails-find-resource
   "javascript: "
   (--map (list it "/\\(.+\\)\\.[^.]+$") projectile-rails-javascript-dirs)))

(defun projectile-rails-find-component ()
  "Find a javascript component."
  (interactive)
  (projectile-rails-find-resource
   "component: "
   `((,projectile-rails-component-dir
      ,(concat projectile-rails-component-dir "\\(.+\\.[^.]+\\)$")))))

(defun projectile-rails-find-stylesheet ()
  "Find a stylesheet file."
  (interactive)
  (projectile-rails-find-resource
   "stylesheet: "
   (--map (list it "/\\(.+\\)\\.[^.]+$") projectile-rails-stylesheet-dirs)))

(defun projectile-rails-find-initializer ()
  "Find an initializer file."
  (interactive)
  (projectile-rails-find-resource
   "initializer: "
   '(("config/initializers/" "config/initializers/\\(.+\\)\\.rb$"))
   "config/initializers/${filename}.rb"))

(defun projectile-rails-find-environment ()
  "Find an environment file."
  (interactive)
  (projectile-rails-find-resource
   "environment: "
   '(("config/" "/\\(application\\|environment\\)\\.rb$") ("config/environments/" "/\\([^/]+\\)\\.rb$"))))

(defun projectile-rails-find-webpack ()
  "Find a webpack configuration"
  (interactive)
  (projectile-rails-find-resource
   "webpack config: "
   '(("config/webpack/" "config/webpack/\\(.+\\.[^.]+\\)$"))))

(defun projectile-rails-find-locale ()
  "Find a locale file."
  (interactive)
  (projectile-rails-find-resource
   "locale: "
   '(("config/locales/"
      "config/locales/\\(.+\\)\\.\\(?:rb\\|yml\\)$"))
   "config/locales/${filename}"))

(defun projectile-rails-find-mailer ()
  "Find a mailer."
  (interactive)
  (projectile-rails-find-resource
   "mailer: "
   '(("app/mailers/" "app/mailers/\\(.+\\)\\.rb$"))
   "app/mailer/${filename}.rb"))

(defun projectile-rails-find-validator ()
  "Find a validator."
  (interactive)
  (projectile-rails-find-resource
   "validator: "
   '(("app/validators/" "/validators/\\(.+?\\)\\(_validator\\)?\\.rb\\'"))
   "app/validators/${filename}_validator.rb"))

(defun projectile-rails-find-job ()
  "Find a job file."
  (interactive)
  (projectile-rails-find-resource
   "job: "
   '(("app/jobs/" "/jobs/\\(.+?\\)\\(_job\\)?\\.rb\\'"))
   "app/jobs/${filename}_job.rb"))

(defun projectile-rails-find-current-model ()
  "Find a model for the current resource."
  (interactive)
  (projectile-rails-find-current-resource "app/models/"
                                          "/${singular}\\.rb$"
                                          'projectile-rails-find-model))

(defun projectile-rails-find-current-controller ()
  "Find a controller for the current resource."
  (interactive)
  (projectile-rails-find-current-resource "app/controllers/"
                                          "app/controllers/\\(.*${plural}\\)_controller\\.rb$"
                                          'projectile-rails-find-controller))

(defun projectile-rails-find-current-serializer ()
  "Find a serializer for the current resource."
  (interactive)
  (projectile-rails-find-current-resource "app/serializers/"
                                          "app/serializers/\\(.*${singular}\\)_serializer\\.rb$"
                                          'projectile-rails-find-serializer))

(defun projectile-rails-find-current-view ()
  "Find a template for the current resource."
  (interactive)
  (projectile-rails-find-current-resource "app/views/"
                                          "/${plural}/\\(.+\\)$"
                                          'projectile-rails-find-view))

(defun projectile-rails-find-current-helper ()
  "Find a helper for the current resource."
  (interactive)
  (projectile-rails-find-current-resource "app/helpers/"
                                          "/\\(${plural}_helper\\)\\.rb$"
                                          'projectile-rails-find-helper))

(defun projectile-rails-find-current-javascript ()
  "Find a javascript for the current resource."
  (interactive)
  (projectile-rails-find-current-resource "app/assets/javascripts/"
                                          "/\\(?:.+/\\)\\(.*${plural}\\)${projectile-rails-javascript-re}"
                                          'projectile-rails-find-javascript))

(defun projectile-rails-find-current-stylesheet ()
  "Find a stylesheet for the current resource."
  (interactive)
  (projectile-rails-find-current-resource "app/assets/stylesheets/"
                                          "/\\(?:.+/\\)\\(.*${plural}\\)${projectile-rails-stylesheet-re}"
                                          'projectile-rails-find-stylesheet))

(defun projectile-rails-find-current-spec ()
  "Find a spec for the current resource."
  (interactive)
  (if (fboundp 'rspec-toggle-spec-and-target)
      (rspec-toggle-spec-and-target)
    (projectile-find-test-file)))

(defun projectile-rails-find-current-test ()
  "Find a test for the current resource."
  (interactive)
  (projectile-toggle-between-implementation-and-test))

(defun projectile-rails-find-current-fixture ()
  "Find a fixture for the current resource."
  (interactive)
  (projectile-rails-find-current-resource
   (first projectile-rails-fixture-dirs)
   "\\(?:test\\|spec\\)/\\(?:fixtures\\|factories\\|fabricators\\)/\\(?:${singular}\\(?:_fabricator\\)?\\|${plural}\\)\\.\\(?:yml\\|rb\\)"
   'projectile-rails-find-fixture))

(defun projectile-rails-find-current-migration ()
  "Find a migration for the current resource."
  (interactive)
  (projectile-rails-find-current-resource "db/migrate/"
                                          "/[0-9]\\{14\\}.*_\\(${plural}\\|${singular}\\).*\\.rb$"
                                          'projectile-rails-find-migration))

(defcustom projectile-rails-resource-name-re-list
  `("/app/models/\\(?:.+/\\)?\\(.+\\)\\.rb\\'"
    "/app/controllers/\\(?:.+/\\)?\\(.+\\)_controller\\.rb\\'"
    "/app/views/\\(?:.+/\\)?\\([^/]+\\)/[^/]+\\'"
    "/app/helpers/\\(?:.+/\\)?\\(.+\\)_helper\\.rb\\'"
    ,(concat "/app/assets/javascripts/\\(?:.+/\\)?\\(.+\\)" projectile-rails-javascript-re)
    ,(concat "/app/assets/stylesheets/\\(?:.+/\\)?\\(.+\\)" projectile-rails-stylesheet-re)
    "/db/migrate/.*create_\\(.+\\)\\.rb\\'"
    "/spec/.*/\\([a-z_]+?\\)\\(?:_controller\\)?_spec\\.rb\\'"
    "/test/.*/\\([a-z_]+?\\)\\(?:_controller\\)?_test\\.rb\\'"
    "/\\(?:test\\|spec\\)/\\(?:fixtures\\|factories\\|fabricators\\)/\\(.+?\\)\\(?:_fabricator\\)?\\.\\(?:yml\\|rb\\)\\'")
  "List of regexps for extracting a resource name from a buffer file name."
  :group 'projectile-rails
  :type '(repeat regexp))

(defun projectile-rails-current-resource-name ()
  "Return a resource name extracted from the name of the currently visiting file."
  (let* ((file-name (buffer-file-name))
         (name (and file-name
                    (loop for re in projectile-rails-resource-name-re-list
                          do (if (string-match re file-name)
                                 (return (match-string 1 file-name)))))))
    (and name
         (singularize-string name))))

(defun projectile-rails-list-entries (fun dir)
  "Call FUN on DIR being a relative directory within a rails project.

It is suspected that the result of FUN will be a list of filepaths.
Each filepath will have the path to the project discarded."
  (--map
   (substring it (length (concat (projectile-rails-root) dir)))
   (funcall fun (projectile-rails-expand-root dir))))

(defun projectile-rails-find-log ()
  "Find a log file.

The opened buffer will have `auto-revert-tail-mode' turned on."
  (interactive)
  (let ((logs-dir (loop for dir in '("log/" "spec/dummy/log/" "test/dummy/log/")
                       until (projectile-rails--file-exists-p dir)
                       finally return dir)))

        (unless logs-dir
          (user-error "No log directory found"))

        ;;logs tend to not be under scm so do not resort to projectile-dir-files
        (find-file (projectile-rails-expand-root
                    (concat
                     logs-dir
                     (projectile-completing-read
                      "log: "
                      (projectile-rails-list-entries 'f-files logs-dir)))))
        (auto-revert-tail-mode +1)
        (setq-local auto-revert-verbose nil)
        (buffer-disable-undo)
        (projectile-rails-on)))

(defun projectile-rails-rake (arg)
  "Run a rake task.

ARG will be passed to `rake' interactive command.
The mode of the output buffer will be `projectile-rails-compilation-mode'."
  (interactive "P")
  (rake arg 'projectile-rails-compilation-mode))

(defvar projectile-rails-cache-data
  (make-hash-table :test 'equal)
  "A hash table that is used for caching information about the current project.")

(defun projectile-rails-cache-key (key)
  "Generate a cache key based on the current directory and the given KEY."
  (format "%s-%s" default-directory key))

(defun projectile-rails-root ()
  "Returns rails root directory if this file is a part of a Rails application else nil"
  (let* ((dir default-directory)
         (cache-key (projectile-rails-cache-key "root"))
         (cache-value (gethash cache-key projectile-rails-cache-data)))
    (or cache-value
        (ignore-errors
          (let ((root (projectile-locate-dominating-file default-directory projectile-rails-root-file)))
            (when (file-exists-p (expand-file-name projectile-rails-verify-root-file root))
              (puthash cache-key root projectile-rails-cache-data)
              root))))))

(defun projectile-rails-root-relative-to-project-root ()
  "Return the location of the rails root relative to `projectile-project-root'."
  (let ((rails-root (file-truename (projectile-rails-root)))
        (project-root (projectile-project-root)))
    (if (string-equal rails-root project-root)
        ""
      (substring rails-root (length (f-common-parent (list rails-root project-root)))))))

(defun projectile-rails-expand-root (dir)
  "Like `projectile-expand-root' but consider `projectile-rails-root'."
  (projectile-expand-root (concat (projectile-rails-root) dir)))

(defun projectile-rails--file-exists-p (filepath)
  "Return t if relative FILEPATH exists within current project."
  (file-exists-p (projectile-rails-expand-root filepath)))

(defun projectile-rails-console (arg)
  "Start a rails console, asking for which if ARG is not nil."
  (interactive "P")
  (projectile-rails-with-root
   (let ((rails-console-command (projectile-rails-with-preloader
                                 :spring (concat projectile-rails-spring-command " rails console")
                                 :zeus "zeus console"
                                 :vanilla (concat projectile-rails-vanilla-command " console"))))
     (with-demoted-errors
         (inf-ruby-console-run
          (if (>= (or (car arg) 0) 4)
              (read-string "rails console: " rails-console-command)
            rails-console-command)
          "rails"))
     (projectile-rails-mode +1))))

;; Shamelessly stolen from rinari.el
(defun projectile-rails--db-config ()
  "Returns contents of config/database.yml as a list"
  (json-read-from-string
   (shell-command-to-string
    (format
     "ruby -ryaml -rjson -e 'JSON.dump(YAML.load(ARGF.read), STDOUT)' \"%s\""
     (projectile-rails-expand-root "config/database.yml")))))

(defvar projectile-rails--sql-adapters->products
  '(("mysql2"         "mysql")
    ("mysql"          "mysql")
    ("jdbcmysql"      "mysql")

    ("postgres"       "postgres")
    ("postgresql"     "postgres")
    ("jdbcpostgresql" "postgres")

    ("sqlite"         "sqlite")
    ("sqlite3"        "sqlite")
    ("jdbcsqlite3"    "sqlite")

    ("informix"       "informix")
    ("ingres"         "ingres")
    ("interbase"      "interbase")
    ("linter"         "linter")
    ("ms"             "ms")
    ("oracle"         "oracle")
    ("solid"          "solid")
    ("sybase"         "sybase")
    ("vertica"        "vertica"))
  "Mapping between ruby libraries and Emacs sql adapters.")

(defun projectile-rails--determine-sql-product (env)
  "Return Emacs sql adapter that should be used for the given project.

ENV is the name of the rails environment."
  (intern
   (car
    (cdr
     (assoc-string (cdr (assoc-string "adapter" (cdr (assoc-string env (projectile-rails--db-config)))))
                   projectile-rails--sql-adapters->products)))))

(defun projectile-rails--choose-env ()
  "Return rails environment to use.

The candidates are based on the files found in config/environments/"
  (projectile-completing-read
   "Choose env: "
   (--map (substring it 0 -3)
    (projectile-rails-list-entries 'f-files "config/environments/"))))

(defun projectile-rails-dbconsole ()
  "Run rails-dbconsole command.

The buffer for interacting with SQL client is created via `sql-product-interactive'."
  (interactive)
  (require 'sql)

  (projectile-rails-with-root
   (let* ((env (projectile-rails--choose-env))
          (product (projectile-rails--determine-sql-product env))
          (sqli-login      (sql-get-product-feature product :sqli-login))
          (sqli-options    (sql-get-product-feature product :sqli-options))
          (sqli-program    (sql-get-product-feature product :sqli-program))
          (sql-comint-func (sql-get-product-feature product :sqli-comint-func))
          (commands (s-split " " (projectile-rails-with-preloader
                                  :spring (concat projectile-rails-spring-command " rails dbconsole")
                                  :zeus (concat projectile-rails-zeus-command " dbconsole")
                                  :vanilla (concat projectile-rails-vanilla-command " dbconsole")))))
     (sql-set-product-feature product :sqli-login '())
     (sql-set-product-feature product :sqli-options '())
     (sql-set-product-feature product :sqli-program (car commands))
     (sql-set-product-feature product :sqli-comint-func (lambda (_ __)
                                                          (sql-comint product (cdr commands))))

     (sql-product-interactive product)

     (sql-set-product-feature product :sqli-comint-func sql-comint-func)
     (sql-set-product-feature product :sqli-program sqli-program)
     (sql-set-product-feature product :sqli-options sqli-options)
     (sql-set-product-feature product :sqli-login sqli-login))))

(defun projectile-rails-expand-snippet-maybe ()
  "Expand snippet corresponding to the current file.

This only works when yas package is installed."
  (when (and projectile-rails-expand-snippet
             (fboundp 'yas-expand-snippet)
             (and (buffer-file-name) (not (file-exists-p (buffer-file-name))))
             (s-blank? (buffer-string))
             (projectile-rails-expand-corresponding-snippet))))

(defun projectile-rails--snippet-for-module (last-part name)
  "Return snippet as string for a file that holds a module."
  (let ((parts (projectile-rails-classify (match-string 1 name))))
    (format
     (concat
      (s-join "" (--map (s-lex-format "module ${it}\n") (butlast parts)))
      last-part
      (s-join "" (make-list (1- (length parts)) "\nend")))
     (-last-item parts))))

(defun projectile-rails--snippet-for-model (name)
  (format
   (if (projectile-rails--file-exists-p "app/models/application_record.rb")
       "class %s < ${1:ApplicationRecord}\n$2\nend"
     "class %s < ${1:ActiveRecord::Base}\n$2\nend")
   (s-join "::" (projectile-rails-classify name))))

(defun projectile-rails--expand-snippet (snippet)
  "Turn on `yas-minor-mode' and expand SNIPPET."
  (yas-minor-mode +1)
  (yas-expand-snippet snippet))

(defun projectile-rails-expand-corresponding-snippet ()
  "Call `projectile-rails--expand-snippet' with a snippet corresponding to the current file."
  (let ((name (buffer-file-name)))
    (cond ((string-match "app/[^/]+/concerns/\\(.+\\)\\.rb$" name)
           (projectile-rails--expand-snippet
            (format
             "module %s\n  extend ActiveSupport::Concern\n  $0\nend"
             (s-join "::" (projectile-rails-classify (match-string 1 name))))))
          ((string-match "app/controllers/\\(.+\\)\\.rb$" name)
           (projectile-rails--expand-snippet
            (format
             "class %s < ${1:ApplicationController}\n$2\nend"
             (s-join "::" (projectile-rails-classify (match-string 1 name))))))
          ((string-match "spec/[^/]+/\\(.+\\)_spec\\.rb$" name)
           (projectile-rails--expand-snippet
            (format
             "require \"${1:rails_helper}\"\n\nRSpec.describe %s do\n  $0\nend"
             (s-join "::" (projectile-rails-classify (match-string 1 name))))))
          ((string-match "app/models/\\(.+\\)\\.rb$" name)
           (projectile-rails--expand-snippet
            (projectile-rails--snippet-for-model (match-string 1 name))))
          ((string-match "app/helpers/\\(.+\\)_helper\\.rb$" name)
           (projectile-rails--expand-snippet
            (format
             "module %sHelper\n$1\nend"
             (s-join "::" (projectile-rails-classify (match-string 1 name))))))
          ((string-match "lib/\\(.+\\)\\.rb$" name)
           (projectile-rails--expand-snippet
            (projectile-rails--snippet-for-module "${1:module} %s\n$2\nend" name)))
          ((string-match "app/\\(?:[^/]+\\)/\\(.+\\)\\.rb$" name)
           (projectile-rails--expand-snippet
            (projectile-rails--snippet-for-module "${1:class} %s\n$2\nend" name))))))

(defun projectile-rails-classify (name)
  "Accepts a filepath, splits it by '/' character and classifieses each of the element"
  (--map (replace-regexp-in-string "_" "" (upcase-initials it)) (split-string name "/")))

(defun projectile-rails-declassify (name)
  "Converts passed string to a relative filepath."
  (let ((case-fold-search nil))
    (downcase
     (replace-regexp-in-string
      "::" "/"
      (replace-regexp-in-string
       " " "_"
       (replace-regexp-in-string
        "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" name))))))

(defun projectile-rails-server ()
  "Runs rails server command"
  (interactive)
  (when (not (projectile-rails--file-exists-p "config/environment.rb"))
    (user-error "You're not running it from a rails application."))
  (if (member projectile-rails-server-buffer-name (mapcar 'buffer-name (buffer-list)))
      (switch-to-buffer projectile-rails-server-buffer-name)
    (projectile-rails-with-root
     (compile (projectile-rails-with-preloader :spring (concat projectile-rails-spring-command " rails server")
                                               :zeus (concat projectile-rails-zeus-command " server")
                                               :vanilla (concat projectile-rails-vanilla-command " server"))
              'projectile-rails-server-mode))))

(defun projectile-rails--completion-in-region ()
  (interactive)
  (let ((generators (--map (concat (car it) " ") projectile-rails-generators)))
    (when (<= (minibuffer-prompt-end) (point))
      (completion-in-region (minibuffer-prompt-end) (point-max)
                            generators))))

(defun projectile-rails--generate-with-completion (command)
  (let ((keymap (copy-keymap minibuffer-local-map)))
    (define-key keymap (kbd "<tab>") 'projectile-rails--completion-in-region)
    (concat command (read-from-minibuffer command nil keymap))))

(defun projectile-rails-generate ()
  "Runs rails generate command"
  (interactive)
  (projectile-rails-with-root
   (let ((command-prefix (projectile-rails-with-preloader
                          :spring (concat projectile-rails-spring-command " rails generate ")
                          :zeus (concat projectile-rails-zeus-command " generate ")
                          :vanilla (concat projectile-rails-vanilla-command " generate "))))
     (compile
      (projectile-rails--generate-with-completion command-prefix)
      'projectile-rails-generate-mode))))

(defun projectile-rails--destroy-read (command)
  (let ((keymap (copy-keymap minibuffer-local-map)))
    (define-key keymap (kbd "<tab>") 'exit-minibuffer)
    (read-from-minibuffer command nil keymap)))

(defun projectile-rails--destroy-with-completion (command)
  (let* ((user-input (projectile-rails--destroy-read command))
         (completion (try-completion user-input
                                     projectile-rails-generators))
         (dirs (cdr (-flatten-n 2 (--filter (string= completion (car it))
                                            projectile-rails-generators))))
         (prompt (concat command completion " ")))
    (if completion
        (concat prompt
                (projectile-completing-read
                 prompt
                 (projectile-rails-hash-keys (projectile-rails-choices dirs))))
      (concat command user-input))))

(defun projectile-rails-destroy ()
  "Runs rails destroy command."
  (interactive)
  (projectile-rails-with-root
   (let ((command-prefix (projectile-rails-with-preloader
                          :spring (concat projectile-rails-spring-command " rails destroy ")
                          :zeus (concat projectile-rails-zeus-command " destroy ")
                          :vanilla (concat projectile-rails-vanilla-command " destroy "))))
     (compile
      (projectile-rails--destroy-with-completion command-prefix)
      'projectile-rails-compilation-mode))))

(defun projectile-rails-sanitize-and-goto-file (dir name &optional ext)
  "Calls `projectile-rails-goto-file' with passed arguments sanitizing them before."
  (projectile-rails-goto-file
   (concat
    (projectile-rails-sanitize-dir-name dir) (projectile-rails-declassify name) ext)))

(defun projectile-rails-goto-file (filepath &optional ask)
  "Finds the FILEPATH after expanding root."
  (projectile-rails-ff (projectile-rails-expand-root filepath) ask))

(defun projectile-rails-goto-gem (gem)
  "Uses `bundle-open' to open GEM. If the function is not defined notifies user."
  (if (not (fboundp 'bundle-open))
      (user-error "Please install bundler.el from https://github.com/tobiassvn/bundler.el")
    (message "Using bundle-open command to open the gem")
    (bundle-open (car (s-split "/" gem)))))

(defun projectile-rails-goto-asset-at-point (dirs)
  "Try to find and go to an asset under the point.

DIRS are directories where to look for assets."
  (let ((name
         (projectile-rails-sanitize-name (thing-at-point 'filename))))
    (projectile-rails-ff
     (loop for dir in dirs
           for re = (s-lex-format "${dir}${name}\\..+$")
           for files = (projectile-dir-files (projectile-rails-expand-root dir))
           for file = (--first (string-match-p re it) files)
           until file
           finally return (and file (projectile-rails-expand-root file))))))

(defun projectile-rails-goto-file-at-point ()
  "Try to find file at point."
  (interactive)
  (let ((name (projectile-rails-name-at-point))
        (line (projectile-rails-current-line))
        (case-fold-search nil))
    (cond ((string-match-p "\\_<render\\_>" line)
           (projectile-rails-goto-template-at-point))

          ((string-match-p "^\\s-*//= require .+\\s-*$" line)
           (projectile-rails-goto-asset-at-point projectile-rails-javascript-dirs))

          ((string-match-p "^\\s-*\\#= require .+\\s-*$" line)
           (projectile-rails-goto-asset-at-point projectile-rails-javascript-dirs))

          ((string-match-p "\\_<javascript_include_tag\\_>" line)
           (projectile-rails-goto-asset-at-point projectile-rails-javascript-dirs))

          ((string-match-p "^\\s-*\\*= require .+\\s-*$" line)
           (projectile-rails-goto-asset-at-point projectile-rails-stylesheet-dirs))

          ((string-match-p "^\\s-*\\@import .+\\s-*$" line)
           (projectile-rails-goto-asset-at-point projectile-rails-stylesheet-dirs))

          ((string-match-p "\\_<stylesheet_link_tag\\_>" line)
           (projectile-rails-goto-asset-at-point projectile-rails-stylesheet-dirs))

          ((string-match-p "\\_<require_relative\\_>" line)
           (projectile-rails-ff (expand-file-name (concat (thing-at-point 'filename) ".rb"))))

          ((string-match-p "\\_<require\\_>" line)
           (projectile-rails-goto-gem (thing-at-point 'filename)))

          ((string-match-p "\\_<gem\\_>" line)
           (projectile-rails-goto-gem (thing-at-point 'filename)))

          ((string-match-p "^[a-z]" name)
           (projectile-rails-find-constant (singularize-string name)))

          ((string-match-p "^\\(::\\)?[A-Z]" name)
           (projectile-rails-goto-constant-at-point)))))

(defun projectile-rails-goto-constant-at-point ()
  "Try to find and go to a Ruby constant at point."
  (let ((bounds (projectile-rails--complete-bounds)))
    (projectile-rails-find-constant (buffer-substring (car bounds) (cdr bounds)))))

;; Stolen from robe
(defun projectile-rails--complete-bounds ()
  (cons
   (save-excursion
     (while (or (not (zerop (skip-syntax-backward "w_")))
                (not (zerop (skip-chars-backward ":")))))
     (point))
   (save-excursion
     (while (or (not (zerop (skip-syntax-forward "w_")))
                (not (zerop (skip-chars-forward ":")))))
     (point))))

(defun projectile-rails-find-constant (name)
  (let* ((code-dirs (-filter #'f-exists? (-map #'projectile-rails-expand-root (projectile-rails--code-directories))))
         (list-parent-dirs (lambda (some-file)
                             (let ((parent-dirs '()))
                               (f-traverse-upwards (lambda (parent-dir)
                                                     (push (f-canonical parent-dir) parent-dirs)
                                                     (equal (f-slash (f-canonical (projectile-rails-root))) (f-slash (f-canonical parent-dir))))
                                                   (f-dirname some-file))
                               parent-dirs)))
         (file-name (format "%s.rb" (projectile-rails-declassify name)))
         (lookup-dirs (if (f-absolute? file-name)
                          ;; If top-level constant (e.g. ::Classname), i.e. derived filename (/classname) starts with a "/", then:
                          ;; Look only in code directories
                          code-dirs
                        (-flatten (list
                                   ;; Otherwise (relative constant):
                                   ;; 1. Look in current file namespace
                                   (f-no-ext buffer-file-name)
                                   ;; 2. Look in local namespace hierarchy
                                   (funcall list-parent-dirs buffer-file-name)
                                   ;; 3. Look in code directories
                                   code-dirs))))
         ;; Strip leading "/" if present before generating lookup paths because it messes with f-join)
         (relative-file-name (if (f-absolute? file-name)
                                 (substring file-name 1)
                               file-name))
         (lookup-paths (--map (f-join it relative-file-name)
                              lookup-dirs))
         (choices
          (-uniq
           (-filter #'f-exists? lookup-paths))))

    (when (= (length choices) 0)
      (user-error "Could not find anything"))

    (cond ((= (length choices) 1)
           (find-file (car choices)))
          ((> (length choices) 1)
           (find-file (projectile-completing-read "Which exactly?: " choices))))))

(defun projectile-rails--code-directories ()
  (let ((app-dirs (projectile-rails-list-entries 'f-directories "app/")))
    (-concat
     (--map (concat "app/" it "/") app-dirs)
     (--map (concat "app/" it "/concerns/") app-dirs)
     '("lib/"))))

(defun projectile-rails--view-p (path)
  (string-prefix-p "app/views/" (s-chop-prefix (projectile-rails-root) path)))

(defun projectile-rails--ignore-buffer-p ()
  "Returns t if `projectile-rails' should not be enabled for the current buffer"
  (string-match-p "\\*\\(Minibuf-[0-9]+\\|helm mini\\)\\*" (buffer-name)))

(defun projectile-rails-extract-region (partial-name)
  "Extract region to a partial called PARTIAL-NAME.

If called interactively will ask user for the PARTIAL-NAME."
  (interactive (list (file-truename (read-file-name "The name of the partial: " default-directory))))
  (let ((projectile-rails-expand-snippet nil)
        (snippet (cdr (assoc (f-ext partial-name) projectile-rails-extracted-region-snippet)))
        (path (replace-regexp-in-string "\/_" "/" (s-chop-prefix
                                                   (projectile-rails-expand-root "app/views/")
                                                   (first (s-slice-at "\\." partial-name))))))
    (kill-region (region-beginning) (region-end))
    (deactivate-mark)
    (when (projectile-rails--view-p (buffer-file-name))
      (insert (format snippet path))
      (indent-according-to-mode)
      (when (not (looking-at-p "\n"))
        (insert "\n")))
    (find-file partial-name)
    (yank)
    (indent-region (point-min) (point-max))))

(defun projectile-rails-template-name (template)
  (-first-item (s-split "\\." (-last-item (s-split "/" template)))))

(defun projectile-rails-template-format (template)
  (let ((at-point-re "\\.\\([^.]+\\)\\.[^.]+$")
        (at-line-re "formats\\(?:'\"\\|:\\)?\\s-*\\(?:=>\\)?\\s-*\\[[:'\"]\\([a-zA-Z0-9]+\\)['\"]?\\]"))
    (cond ((string-match at-point-re template)
           (match-string 1 template))
          ((string-match at-line-re (projectile-rails-current-line))
           (match-string 1 (projectile-rails-current-line)))
          (t
           (when (string-match at-point-re (buffer-file-name))
             (match-string 1 (buffer-file-name)))))))

(defun projectile-rails-template-dir (template)
  (projectile-rails-sanitize-dir-name
   (cond ((string-match "\\(.+\\)/[^/]+$" template)
          (projectile-rails-expand-root
           (concat "app/views/" (match-string 1 template))))
         ((string-match "app/controllers/\\(.+\\)_controller\\.rb$" (buffer-file-name))
          (projectile-rails-expand-root
           (concat "app/views/" (match-string 1 (buffer-file-name)))))
         (t
          default-directory))))

(defun projectile-rails--goto-template-at-point (dir name format)
  (loop for processor in '("erb" "haml" "slim")
        for template = (s-lex-format "${dir}${name}.${format}.${processor}")
        for partial = (s-lex-format "${dir}_${name}.${format}.${processor}")
        until (or
               (projectile-rails-ff template)
               (projectile-rails-ff partial))))

(defun projectile-rails-goto-template-at-point ()
  "Visit a template or a partial under the point."
  (interactive)
  (let* ((template (projectile-rails-filename-at-point))
         (dir (projectile-rails-template-dir template))
         (name (projectile-rails-template-name template))
         (format (projectile-rails-template-format template)))
    (if format
        (or (projectile-rails--goto-template-at-point dir name format)
            (projectile-rails--goto-template-at-point (projectile-rails-expand-root "app/views/application/")
                                                      name
                                                      format))
      (message "Could not recognize the template's format")
      (dired dir))))

(defun projectile-rails-goto-gemfile ()
  "Visit Gemfile"
  (interactive)
  (projectile-rails-goto-file "Gemfile"))

(defun projectile-rails-goto-package ()
  "Visit package.json file."
  (interactive)
  (projectile-rails-goto-file "package.json"))

(defun projectile-rails-goto-schema ()
  "Visit db/schema.rb file."
  (interactive)
  (projectile-rails-goto-file "db/schema.rb"))

(defun projectile-rails-goto-seeds ()
  "Visit db/seed.rb file."
  (interactive)
  (projectile-rails-goto-file "db/seeds.rb"))

(defun projectile-rails-goto-routes ()
  "Visit config/routes.rb file."
  (interactive)
  (projectile-rails-goto-file "config/routes.rb"))

(defun projectile-rails-goto-spec-helper ()
  "Visit spec/spec_helper.rb file."
  (interactive)
  (projectile-rails-goto-file "spec/spec_helper.rb"))

(defun projectile-rails-ff (path &optional ask)
  "Calls `find-file' function on PATH when it is not nil and the file exists.

If file does not exist and ASK in not nil it will ask user to proceed."
  (if (or (and path (file-exists-p path))
          (and ask (yes-or-no-p (s-lex-format "File does not exists. Create a new buffer ${path} ?"))))
      (find-file path)))

(defun projectile-rails-name-at-point ()
  (projectile-rails-sanitize-name (symbol-name (symbol-at-point))))

(defun projectile-rails-filename-at-point ()
  (projectile-rails-sanitize-name (thing-at-point 'filename)))

(defun projectile-rails-apply-ansi-color ()
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))

(defun projectile-rails--log-buffer-find-template (button)
  (projectile-rails-sanitize-and-goto-file "app/views/" (button-label button)))

(defun projectile-rails--log-buffer-find-controller (button)
  (projectile-rails-sanitize-and-goto-file "app/controllers/" (button-label button) ".rb"))

(defun projectile-rails--generate-buffer-make-buttons (buffer exit-code)
  (with-current-buffer buffer
    (goto-char 0)
    (while (re-search-forward projectile-rails-generate-filepath-re (max-char) t)
      (make-button
       (match-beginning 1)
       (match-end 1)
       'action
       'projectile-rails-generate-ff
       'follow-link
       t))))

(defun projectile-rails-server-make-buttons ()
  (projectile-rails--log-buffer-make-buttons compilation-filter-start (point)))

(defun projectile-rails--log-buffer-make-buttons (start end)
  (save-excursion
    (goto-char start)
    (while (not (= (point) end))
      (cond ((re-search-forward "Rendered \\([^ ]+\\)" (line-end-position) t)
             (make-button (match-beginning 1) (match-end 1) 'action 'projectile-rails--log-buffer-find-template 'follow-link t))
            ((re-search-forward "Processing by \\(.+\\)#\\(?:[^ ]+\\)" (line-end-position) t)
             (make-button (match-beginning 1) (match-end 1) 'action 'projectile-rails--log-buffer-find-controller 'follow-link t)))
      (forward-line))))

(defun projectile-rails-server-terminate ()
  (let ((process (get-buffer-process projectile-rails-server-buffer-name)))
    (when process (signal-process process 15))))

(defun projectile-rails-generate-ff (button)
  (find-file (projectile-rails-expand-root (button-label button))))

(defun projectile-rails-sanitize-name (name)
  (when (or
         (and (s-starts-with? "'" name) (s-ends-with? "'" name))
         (and (s-starts-with? "\"" name) (s-ends-with? "\"" name)))
    (setq name (substring name 1 -1)))
  (when (s-starts-with? "./" name)
    (setq name (substring name 2)))
  (when (or (string-match-p "^:[^:]" name) (s-starts-with? "/" name))
    (setq name (substring name 1)))
  (when (s-ends-with? "," name)
    (setq name (substring name 0 -1)))
  name)

(defun projectile-rails-sanitize-dir-name (name)
  (if (s-ends-with? "/" name) name (concat name "/")))

(defun projectile-rails-current-line ()
  (save-excursion
    (let (beg)
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (buffer-substring-no-properties beg (point)))))

(defun projectile-rails-set-assets-dirs ()
  (setq-local
   projectile-rails-javascript-dirs
   (--filter (projectile-rails--file-exists-p it) projectile-rails-javascript-dirs))
  (setq-local
   projectile-rails-stylesheet-dirs
   (--filter (projectile-rails--file-exists-p it) projectile-rails-stylesheet-dirs)))


(defun projectile-rails-set-fixture-dirs ()
  (setq-local
   projectile-rails-fixture-dirs
   (--filter (file-exists-p (projectile-rails-expand-root it)) projectile-rails-fixture-dirs)))

(defvar projectile-rails-mode-goto-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'projectile-rails-goto-file-at-point)
    (define-key map (kbd "g") 'projectile-rails-goto-gemfile)
    (define-key map (kbd "r") 'projectile-rails-goto-routes)
    (define-key map (kbd "d") 'projectile-rails-goto-schema)
    (define-key map (kbd "s") 'projectile-rails-goto-seeds)
    (define-key map (kbd "h") 'projectile-rails-goto-spec-helper)
    (define-key map (kbd "p") 'projectile-rails-goto-package)
    map)
  "A goto map for `projectile-rails-mode'.")
(fset 'projectile-rails-mode-goto-map projectile-rails-mode-goto-map)

(defvar projectile-rails-mode-run-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'projectile-rails-console)
    (define-key map (kbd "s") 'projectile-rails-server)
    (define-key map (kbd "r") 'projectile-rails-rake)
    (define-key map (kbd "g") 'projectile-rails-generate)
    (define-key map (kbd "d") 'projectile-rails-destroy)
    (define-key map (kbd "b") 'projectile-rails-dbconsole)
    map)
  "A run map for `projectile-rails-mode'.")
(fset 'projectile-rails-mode-run-map projectile-rails-mode-run-map)

(defvar projectile-rails-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'projectile-rails-find-model)
    (define-key map (kbd "M") 'projectile-rails-find-current-model)

    (define-key map (kbd "c") 'projectile-rails-find-controller)
    (define-key map (kbd "C") 'projectile-rails-find-current-controller)

    (define-key map (kbd "v") 'projectile-rails-find-view)
    (define-key map (kbd "V") 'projectile-rails-find-current-view)

    (define-key map (kbd "j") 'projectile-rails-find-javascript)
    (define-key map (kbd "J") 'projectile-rails-find-current-javascript)

    (define-key map (kbd "s") 'projectile-rails-find-stylesheet)
    (define-key map (kbd "S") 'projectile-rails-find-current-stylesheet)

    (define-key map (kbd "h") 'projectile-rails-find-helper)
    (define-key map (kbd "H") 'projectile-rails-find-current-helper)

    (define-key map (kbd "p") 'projectile-rails-find-spec)
    (define-key map (kbd "P") 'projectile-rails-find-current-spec)

    (define-key map (kbd "t") 'projectile-rails-find-test)
    (define-key map (kbd "T") 'projectile-rails-find-current-test)

    (define-key map (kbd "n") 'projectile-rails-find-migration)
    (define-key map (kbd "N") 'projectile-rails-find-current-migration)

    (define-key map (kbd "r") 'projectile-rails-console)
    (define-key map (kbd "R") 'projectile-rails-server)

    (define-key map (kbd "u") 'projectile-rails-find-fixture)
    (define-key map (kbd "U") 'projectile-rails-find-current-fixture)

    (define-key map (kbd "w") 'projectile-rails-find-component)

    (define-key map (kbd "l") 'projectile-rails-find-lib)
    (define-key map (kbd "f") 'projectile-rails-find-feature)
    (define-key map (kbd "i") 'projectile-rails-find-initializer)
    (define-key map (kbd "o") 'projectile-rails-find-log)
    (define-key map (kbd "e") 'projectile-rails-find-environment)
    (define-key map (kbd "W") 'projectile-rails-find-webpack)
    (define-key map (kbd "a") 'projectile-rails-find-locale)
    (define-key map (kbd "@") 'projectile-rails-find-mailer)
    (define-key map (kbd "!") 'projectile-rails-find-validator)
    (define-key map (kbd "y") 'projectile-rails-find-layout)
    (define-key map (kbd "k") 'projectile-rails-find-rake-task)
    (define-key map (kbd "b") 'projectile-rails-find-job)

    (define-key map (kbd "z") 'projectile-rails-find-serializer)
    (define-key map (kbd "Z") 'projectile-rails-find-current-serializer)
    ;; (define-key map (kbd "?") 'projectile-rails-find-validator)

    (define-key map (kbd "x") 'projectile-rails-extract-region)
    (define-key map (kbd "RET") 'projectile-rails-goto-file-at-point)

    (define-key map (kbd "g") 'projectile-rails-mode-goto-map)
    (define-key map (kbd "!") 'projectile-rails-mode-run-map)
    map)
  "Keymap after `projectile-rails-keymap-prefix'.")
(fset 'projectile-rails-command-map projectile-rails-command-map)

(defvar projectile-rails-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map projectile-rails-keymap-prefix 'projectile-rails-command-map)
    map)
  "Keymap for `projectile-rails-mode'.")

(easy-menu-define projectile-rails-menu projectile-rails-mode-map
  "Menu for `projectile-rails-mode'."
  '("Rails"
    ["Find model"               projectile-rails-find-model]
    ["Find controller"          projectile-rails-find-controller]
    ["Find view"                projectile-rails-find-view]
    ["Find javascript"          projectile-rails-find-javascript]
    ["Find component"           projectile-rails-find-component]
    ["Find stylesheet"          projectile-rails-find-stylesheet]
    ["Find helper"              projectile-rails-find-helper]
    ["Find spec"                projectile-rails-find-spec]
    ["Find test"                projectile-rails-find-test]
    ["Find feature"             projectile-rails-find-feature]
    ["Find migration"           projectile-rails-find-migration]
    ["Find fixture"             projectile-rails-find-fixture]
    ["Find lib"                 projectile-rails-find-lib]
    ["Find initializer"         projectile-rails-find-initializer]
    ["Find environment"         projectile-rails-find-environment]
    ["Find webpack config"      projectile-rails-find-webpack]
    ["Find log"                 projectile-rails-find-log]
    ["Find locale"              projectile-rails-find-locale]
    ["Find mailer"              projectile-rails-find-mailer]
    ["Find validator"           projectile-rails-find-validator]
    ["Find layout"              projectile-rails-find-layout]
    ["Find rake task"           projectile-rails-find-rake-task]
    ["Find job"                 projectile-rails-find-job]
    ["Find serializer"          projectile-rails-find-serializer]
    "--"
    ["Go to file at point"      projectile-rails-goto-file-at-point]
    "--"
    ["Go to Gemfile"            projectile-rails-goto-gemfile]
    ["Go to package"            projectile-rails-goto-package]
    ["Go to routes"             projectile-rails-goto-routes]
    ["Go to schema"             projectile-rails-goto-schema]
    ["Go to seeds"              projectile-rails-goto-seeds]
    ["Go to spec helper"        projectile-rails-goto-spec-helper]
    "--"
    ["Go to current model"      projectile-rails-find-current-model]
    ["Go to current controller" projectile-rails-find-current-controller]
    ["Go to current view"       projectile-rails-find-current-view]
    ["Go to current javascript" projectile-rails-find-current-javascript]
    ["Go to current stylesheet" projectile-rails-find-current-stylesheet]
    ["Go to current spec"       projectile-rails-find-current-spec]
    ["Go to current test"       projectile-rails-find-current-test]
    ["Go to current migration"  projectile-rails-find-current-migration]
    ["Go to current fixture"    projectile-rails-find-current-fixture]
    ["Go to current serializer" projectile-rails-find-current-serializer]
    "--"
    ["Extract to partial"       projectile-rails-extract-region]
    "--"
    ["Run console"              projectile-rails-console]
    ["Run dbconsole"            projectile-rails-dbconsole]
    ["Run server"               projectile-rails-server]
    ["Run rake"                 projectile-rails-rake]
    ["Run rails generate"       projectile-rails-generate]
    ["Run rails destroy"        projectile-rails-destroy]))

;;;###autoload
(define-minor-mode projectile-rails-mode
  "Rails mode based on projectile"
  :init-value nil
  :lighter " Rails"
  (when projectile-rails-mode
    (and projectile-rails-expand-snippet (projectile-rails-expand-snippet-maybe))
    (and projectile-rails-add-keywords (projectile-rails-add-keywords-for-file-type))
    (projectile-rails-set-assets-dirs)
    (projectile-rails-set-fixture-dirs)))

;;;###autoload
(defun projectile-rails-on ()
  "Enable `projectile-rails-mode' minor mode if this is a rails project."
  (when (and
         (projectile-project-p)
         (not (projectile-rails--ignore-buffer-p))
         (projectile-rails-root))
    (projectile-rails-mode +1)))

;;;###autoload
(define-globalized-minor-mode projectile-rails-global-mode
  projectile-rails-mode
  projectile-rails-on)

(defun projectile-rails-off ()
  "Disable `projectile-rails-mode' minor mode."
  (projectile-rails-mode -1))

(define-derived-mode projectile-rails-server-mode compilation-mode "Projectile Rails Server"
  "Compilation mode for running rails server used by `projectile-rails'.

Killing the buffer will terminate to server's process."
  (set (make-local-variable 'compilation-error-regexp-alist) (list 'ruby-Test::Unit))
  (add-hook 'compilation-filter-hook 'projectile-rails-server-make-buttons nil t)
  (when projectile-rails-server-mode-ansi-colors
    (add-hook 'compilation-filter-hook 'projectile-rails-apply-ansi-color nil t))
  (add-hook 'kill-buffer-hook 'projectile-rails-server-terminate t t)
  (add-hook 'kill-emacs-hook 'projectile-rails-server-terminate t t)
  (setq-local compilation-scroll-output t)
  (projectile-rails-mode +1))

(define-derived-mode projectile-rails-compilation-mode compilation-mode "Projectile Rails Compilation"
  "Compilation mode used by `projectile-rails'."
  (add-hook 'compilation-filter-hook 'projectile-rails-apply-ansi-color nil t)
  (projectile-rails-mode +1))

(define-derived-mode projectile-rails-generate-mode projectile-rails-compilation-mode "Projectile Rails Generate"
  "Mode for output of rails generate."
  (add-hook 'compilation-finish-functions 'projectile-rails--generate-buffer-make-buttons nil t)
  (projectile-rails-mode +1))

(when (functionp 'discover-add-context-menu)

  (defun projectile-rails--discover-find-submenu ()
    (interactive)
    (call-interactively
     (discover-get-context-menu-command-name 'projectile-rails-find)))

  (defun projectile-rails--discover-goto-submenu ()
    (interactive)
    (call-interactively
     (discover-get-context-menu-command-name 'projectile-rails-goto)))

  (defun projectile-rails--discover-run-submenu ()
    (interactive)
    (call-interactively
     (discover-get-context-menu-command-name 'projectile-rails-run)))

  (discover-add-context-menu
   :context-menu '(projectile-rails-mode
                   (description "Mode for Rails projects")
                   (actions
                    ("Available"
                     ("f" "find resources"   projectile-rails--discover-find-submenu)
                     ("g" "goto resources"   projectile-rails--discover-goto-submenu)
                     ("r" "run and interact" projectile-rails--discover-run-submenu))))
   :bind projectile-rails-discover-bind
   :mode 'projectile-rails
   :mode-hook 'projectile-rails-mode-hook)

  (discover-add-context-menu
   :context-menu '(projectile-rails-find
                   (description "Find resources")
                   (actions
                    ("Find a resource"
                     ("m" "model"       projectile-rails-find-model)
                     ("v" "view"        projectile-rails-find-view)
                     ("c" "controller"  projectile-rails-find-controller)
                     ("h" "helper"      projectile-rails-find-helper)
                     ("l" "lib"         projectile-rails-find-lib)
                     ("j" "javascript"  projectile-rails-find-javascript)
                     ("w" "component"   projectile-rails-find-component)
                     ("s" "stylesheet"  projectile-rails-find-stylesheet)
                     ("p" "spec"        projectile-rails-find-spec)
                     ("u" "fixture"     projectile-rails-find-fixture)
                     ("t" "test"        projectile-rails-find-test)
                     ("f" "feature"     projectile-rails-find-feature)
                     ("i" "initializer" projectile-rails-find-initializer)
                     ("o" "log"         projectile-rails-find-log)
                     ("@" "mailer"      projectile-rails-find-mailer)
                     ("!" "validator"   projectile-rails-find-validator)
                     ("y" "layout"      projectile-rails-find-layout)
                     ("n" "migration"   projectile-rails-find-migration)
                     ("k" "rake task"   projectile-rails-find-rake-task)
                     ("b" "job"         projectile-rails-find-job)
                     ("z" "serializer"  projectile-rails-find-serializer)
                     ("a" "locale"      projectile-rails-find-locale))
                    ("Find an associated resource"
                     ("M" "model"       projectile-rails-find-current-model)
                     ("V" "view"        projectile-rails-find-current-view)
                     ("C" "controller"  projectile-rails-find-current-controller)
                     ("H" "helper"      projectile-rails-find-current-helper)
                     ("J" "javascript"  projectile-rails-find-current-javascript)
                     ("S" "stylesheet"  projectile-rails-find-current-stylesheet)
                     ("P" "spec"        projectile-rails-find-current-spec)
                     ("U" "fixture"     projectile-rails-find-current-fixture)
                     ("T" "test"        projectile-rails-find-current-test)
                     ("N" "migration"   projectile-rails-find-current-migration)
                     ("Z" "serializer"  projectile-rails-find-current-serializer))))
   :bind "") ;;accessible only from the main context menu

  (discover-add-context-menu
   :context-menu '(projectile-rails-goto
                   (description "Go to a specific file")
                   (actions
                    ("Go to"
                     ("f" "file at point" projectile-rails-goto-file-at-point)
                     ("g" "Gemfile"       projectile-rails-goto-gemfile)
                     ("p" "package"       projectile-rails-goto-package)
                     ("r" "routes"        projectile-rails-goto-routes)
                     ("d" "schema"        projectile-rails-goto-schema)
                     ("s" "seeds"         projectile-rails-goto-seeds)
                     ("h" "spec helper"   projectile-rails-goto-spec-helper))))
   :bind "") ;;accessible only from the main context menu

  (discover-add-context-menu
   :context-menu '(projectile-rails-run
                   (description "Run and interact")
                   (actions
                    ("Run external command"
                     ("r" "rake"           projectile-rails-rake)
                     ("c" "console"        projectile-rails-console)
                     ("b" "dbconsole"      projectile-rails-dbconsole)
                     ("s" "server"         projectile-rails-server)
                     ("g" "generate"       projectile-rails-generate)
                     ("d" "destroy"        projectile-rails-destroy))
                    ("Interact"
                     ("x" "extract region" projectile-rails-extract-region))))
   :bind "")) ;;accessible only from the main context menu

(with-no-warnings
  (ignore-errors
    (defhydra hydra-projectile-rails-find (:color blue :columns 8)
      "Find a resources"
      ("m" projectile-rails-find-model       "model")
      ("v" projectile-rails-find-view        "view")
      ("c" projectile-rails-find-controller  "controller")
      ("h" projectile-rails-find-helper      "helper")
      ("l" projectile-rails-find-lib         "lib")
      ("j" projectile-rails-find-javascript  "javascript")
      ("w" projectile-rails-find-component  "component")
      ("s" projectile-rails-find-stylesheet  "stylesheet")
      ("p" projectile-rails-find-spec        "spec")
      ("u" projectile-rails-find-fixture     "fixture")
      ("t" projectile-rails-find-test        "test")
      ("f" projectile-rails-find-feature     "feature")
      ("i" projectile-rails-find-initializer "initializer")
      ("o" projectile-rails-find-log         "log")
      ("@" projectile-rails-find-mailer      "mailer")
      ("!" projectile-rails-find-validator   "validator")
      ("y" projectile-rails-find-layout      "layout")
      ("n" projectile-rails-find-migration   "migration")
      ("k" projectile-rails-find-rake-task   "rake task")
      ("b" projectile-rails-find-job         "job")
      ("z" projectile-rails-find-serializer  "serializer")

      ("M" projectile-rails-find-current-model      "current model")
      ("V" projectile-rails-find-current-view       "current view")
      ("C" projectile-rails-find-current-controller "current controller")
      ("H" projectile-rails-find-current-helper     "current helper")
      ("J" projectile-rails-find-current-javascript "current javascript")
      ("S" projectile-rails-find-current-stylesheet "current stylesheet")
      ("P" projectile-rails-find-current-spec       "current spec")
      ("U" projectile-rails-find-current-fixture    "current fixture")
      ("T" projectile-rails-find-current-test       "current test")
      ("N" projectile-rails-find-current-migration  "current migration")
      ("Z" projectile-rails-find-current-serializer "current serializer"))

    (defhydra hydra-projectile-rails-goto (:color blue :columns 8)
      "Go to"
      ("f" projectile-rails-goto-file-at-point "file at point")
      ("g" projectile-rails-goto-gemfile       "Gemfile")
      ("p" projectile-rails-goto-package       "package")
      ("r" projectile-rails-goto-routes        "routes")
      ("d" projectile-rails-goto-schema        "schema")
      ("s" projectile-rails-goto-seeds         "seeds")
      ("h" projectile-rails-goto-spec-helper   "spec helper"))

    (defhydra hydra-projectile-rails-run (:color blue :columns 8)
      "Run external command & interact"
      ("r" projectile-rails-rake       "rake")
      ("c" projectile-rails-console    "console")
      ("b" projectile-rails-dbconsole  "dbconsole")
      ("s" projectile-rails-server     "server")
      ("g" projectile-rails-generate   "generate")
      ("d" projectile-rails-destroy    "destroy")
      ("x" projectile-rails-extract-region "extract region"))

    (defhydra hydra-projectile-rails (:color blue :columns 8)
      "Projectile Rails"
      ("f" hydra-projectile-rails-find/body "Find a resource")
      ("g" hydra-projectile-rails-goto/body "Goto")
      ("r" hydra-projectile-rails-run/body "Run & interact"))))

(provide 'projectile-rails)

;;; projectile-rails.el ends here
