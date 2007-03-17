;;; rails-scripts.el --- emacs-rails integraions with rails script/* scripts

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

(defvar rails-generation-buffer-name "*RailsGeneration*")
(defvar rails-rake-tests-alist
  '(("all"         . "test")
    ("recent"      . "test:recent")
    ("units"       . "test:units")
    ("functionals" . "test:functionals")
    ("integraion"  . "test:integration")))

(defvar rails-rake-recent-test-alist nil)

(defvar rails-generators-list
  '("controller" "model" "scaffold" "migration" "plugin" "mailer" "observer" "resource"))

(defvar rails-destroy-list
  '("controller" "model" "scaffold" "migration" "plugin" "mailer" "observer" "resource"))

(defvar rails-generate-params-list
  '("-f")
  "Add parameters to script/generate.
For example -s to keep existing files and -c to add new files into svn.")

(defvar rails-destroy-params-list
  '("-f")
  "Add parameters to script/destroy.
For example -c to remove files from svn.")

(defconst rails-rake-test-error-regexp-alist
  '(("^[ \t]+[0-9]+) \\(Error\\|Failure\\)") 1 2))

(defun rails-run-script (script buffer parameters &optional message-format)
  "Run a Rails script with PARAMETERS in BUFFER using
MESSAGE-FORMAT to format the output."
  (rails-core:with-root
   (root)
   (let ((default-directory root))
     (rails-logged-shell-command
      (apply #'concat (format "script/%s " script)
             (mapcar #'(lambda (str)
                         (if str (concat str " ") ""))
                     parameters))
      buffer))
  (when message-format
    (message message-format (capitalize (first parameters))
             (second parameters)))))

;;;;;;;;;; Destroy stuff ;;;;;;;;;;

(defun rails-destroy-run (&rest parameters)
  "Run the destroy script."
  (rails-run-script "destroy" rails-generation-buffer-name
                    (append parameters rails-destroy-params-list)
                    "%s %s destroyed."))

(defun rails-destroy (&optional what)
  "Run destroy WHAT"
  (interactive (list (completing-read "What destroy? (use autocomplete): " rails-destroy-list)))
  (let ((name (intern (concat "rails-destroy-" what))))
    (when (fboundp name)
      (call-interactively name))))

(defun rails-destroy-controller (&optional controller-name)
  "Run the destroy script for controllers."
  (interactive
   (list (completing-read "Destroy controller: " (list->alist (rails-core:controllers t)))))
  (when (string-not-empty controller-name)
    (rails-destroy-run "controller" controller-name)))

(defun rails-destroy-model (&optional model-name)
  "Run the destroy script for models."
  (interactive (list (completing-read "Destroy model: " (list->alist (rails-core:models)))))
  (when (string-not-empty model-name)
    (rails-destroy-run "model" model-name)))

(defun rails-destroy-scaffold (&optional scaffold-name)
  "Run the destroy script for scaffolds."
  ;; buggy
  (interactive "MDestroy scaffold: ")
  (when (string-not-empty scaffold-name)
    (rails-destroy-run "scaffold" scaffold-name)))

(defun rails-destroy-migration (&optional migration-name)
  "Run the destroy script for migration"
  (interactive (list (completing-read "Destroy migration: " (list->alist (rails-core:migrations)))))
  (when (string-not-empty migration-name)
    (rails-destroy-run "migration" migration-name)))

(defun rails-destroy-mailer (&optional mailer-name)
  "Run the destroy script for mailer"
  (interactive "MDestroy mailer: ")
  (when (string-not-empty mailer-name)
    (rails-destroy-run "mailer" mailer-name)))

(defun rails-destroy-plugin (&optional plugin-name)
  "Run the destroy script for plugin"
  (interactive (list (completing-read "Destroy plugin: " (list->alist (rails-core:plugins)))))
  (when (string-not-empty plugin-name)
    (rails-destroy-run "plugin" plugin-name)))

(defun rails-destroy-observer (&optional observer-name)
  "Run the destroy script for observer"
  (interactive "MDestroy observer: ")
  (when (string-not-empty observer-name)
    (rails-destroy-run "observer" observer-name)))

(defun rails-destroy-resource (&optional resource-name)
  "Run the destroy script for resource"
  (interactive "MDestroy resource: ")
  (when (string-not-empty resource-name)
    (rails-destroy-run "resource" resource-name)))

;;;;;;;;;; Generators stuff ;;;;;;;;;;

(defun rails-generate-run (&rest parameters)
  "Run the generate script using PARAMETERS."
  (rails-run-script "generate"
                    rails-generation-buffer-name
                    (append parameters rails-generate-params-list)
                    "%s %s generated."))

(defun rails-generate (&optional what)
  "Run generate WHAT"
  (interactive (list (completing-read "What generate? (use autocomplete): " rails-generators-list)))
  (let ((name (intern (concat "rails-generate-" what))))
    (when (fboundp name)
      (call-interactively name))))

(defun rails-generate-controller (&optional controller-name actions)
  "Generate a controller and open the controller file."
  (interactive (list
                (completing-read "Controller name (use autocomplete) : "
                                 (list->alist (rails-core:controllers-ancestors)))
                (read-string "Actions (or return to skip): ")))
  (when (string-not-empty controller-name)
    (rails-generate-run "controller" controller-name actions)
    (rails-core:find-file-if-exist (rails-core:controller-file controller-name))))

(defun rails-generate-model (&optional model-name)
  "Generate a model and open the model file."
  (interactive
   (list (completing-read "Model name: " (list->alist (rails-core:models-ancestors)))))
  (when (string-not-empty model-name)
    (rails-generate-run "model" model-name)
    (rails-core:find-file-if-exist (rails-core:model-file model-name))))

(defun rails-generate-scaffold (&optional model-name controller-name actions)
  "Generate a scaffold and open the controller file."
  (interactive
   "MModel name: \nMController (or return to skip): \nMActions (or return to skip): ")
  (when (string-not-empty model-name)
    (if (string-not-empty controller-name)
        (progn
          (rails-generate-run "scaffold" model-name controller-name actions)
          (rails-core:find-file-if-exist (rails-core:controller-file controller-name)))
      (progn
        (rails-generate-run "scaffold" model-name)
        (rails-core:find-file-if-exist (rails-core:controller-file model-name))))))

(defun rails-generate-migration (migration-name)
  "Generate a migration and open the migration file."
  (interactive "MMigration name: ")
  (when (string-not-empty migration-name)
    (rails-generate-run "migration" migration-name)
    (rails-core:find-file-if-exist
     (save-excursion
       (set-buffer rails-generation-buffer-name)
       (goto-line 2)
       (search-forward-regexp "\\(db/migrate/[0-9a-z_]+.rb\\)")
       (match-string 1)))))

(defun rails-generate-plugin (plugin-name)
  "Generate a plugin and open the init.rb file."
  (interactive "MPlugin name: ")
  (when (string-not-empty plugin-name)
    (rails-generate-run "plugin" plugin-name)
    (rails-core:find-file-if-exist (concat "vendor/plugins/" plugin-name "/init.rb"))))

(defun rails-generate-mailer (mailer-name)
  "Generate a mailer and open the mailer file"
  (interactive "MMailer name: ")
  (when (string-not-empty mailer-name)
    (rails-generate-run "mailer" mailer-name)
    (rails-core:find-file-if-exist (concat (rails-core:model-file mailer-name)))))

(defun rails-generate-observer (observer-name)
  "Generate a observer and open the observer file"
  (interactive "MObserver name: ")
  (when (string-not-empty observer-name)
    (rails-generate-run "observer" observer-name)
    (unless (string-match "[Oo]bserver$" observer-name)
      (setq observer-name (concat observer-name "_observer")))
    (rails-core:find-file-if-exist (concat (rails-core:model-file observer-name)))))

(defun rails-generate-resource (resource-name)
  "Generate a resource and open the resource file"
  (interactive "MResource name: ")
  (when (string-not-empty resource-name)
    (rails-generate-run "resource" resource-name)
    ;; pluralize bug
    (rails-core:find-file-if-exist (concat (rails-core:controller-file resource-name)))))

;;;;;;;;;; Rails create project ;;;;;;;;;;

(defun rails-create-project (dir)
  "Create a new project in a directory named DIR."
  (interactive "FNew project directory: ")
  (shell-command (concat "rails " dir)
                 rails-generation-buffer-name)
  (flet ((rails-core:root () (concat dir "/") ))
    (rails-log-add
     (format "\nCreating project %s\n%s"
             dir (buffer-string-by-name rails-generation-buffer-name))))
  (find-file dir))

;;;;;;;;;; Shells ;;;;;;;;;;

(defun run-ruby-in-buffer (cmd buf)
  "Run CMD as a ruby process in BUF if BUF does not exist."
  (let ((abuf (concat "*" buf "*")))
    (if (not (comint-check-proc abuf))
  (set-buffer (make-comint buf rails-ruby-command nil cmd)))
    (inferior-ruby-mode)
    (make-local-variable 'inferior-ruby-first-prompt-pattern)
    (make-local-variable 'inferior-ruby-prompt-pattern)
    (setq inferior-ruby-first-prompt-pattern "^>> "
          inferior-ruby-prompt-pattern "^>> ")
    (pop-to-buffer abuf)))

(defun rails-interactive-buffer-name (name)
  "Return a buffer name in the format
*rails-<project-name>-<name>*."
  (format "rails-%s-%s" (rails-core:project-name) name))

(defun rails-run-interactive (name script)
  "Run an interactive shell with SCRIPT in a buffer named
*rails-<project-name>-<name>*."
  (rails-core:with-root
   (root)
   (run-ruby-in-buffer (rails-core:file script)
                       (rails-interactive-buffer-name name))
   (rails-minor-mode t)))

(defun rails-run-console ()
  "Run script/console."
  (interactive)
  (rails-run-interactive "console" "script/console"))

(defun rails-run-breakpointer ()
  "Run script/breakpointer."
  (interactive)
  (rails-run-interactive "breakpointer" "script/breakpointer"))

;;;; Rake ;;;;

(defun rails-rake-create-cache (file-name)
  "Create a cache file from rake --tasks output."
  (write-string-to-file file-name
   (prin1-to-string
    (loop for str in (split-string (shell-command-to-string "rake --tasks") "\n")
          for task = (when (string-not-empty str)
                       (string-match "^rake \\([^ ]*\\).*# \\(.*\\)" str)
                       (match-string 1 str))
          when task collect task))))

(defun rails-rake-tasks ()
  "Return all tasks in the main Rails Rakefile."
  (rails-core:in-root
   (let ((cache-file (rails-core:file ".rake-tasks-cache")))
     (unless (file-exists-p cache-file)
       (rails-rake-create-cache cache-file))
     (read-from-file cache-file))))

(defun rails-rake (&optional task message)
  "Run a Rake task in RAILS_ROOT."
  (interactive (list (completing-read "Rake task (use autocomplete): " (list->alist (rails-rake-tasks)))))
  (save-some-buffers)
  (rails-core:in-root
   (message (or message (format "Running rake task \"%s\"" task)))
   (shell-command (concat "rake " task) "*Rails Rake Output*" "*Rails Rake Errors*")))

(defun rails-rake-tests (&optional what)
  "Run Rake tests in RAILS_ROOT."
  (interactive (list (completing-read (concat "What test run?"
                                              (when rails-rake-recent-test-alist
                                                (concat " (" rails-rake-recent-test-alist  ")") )
                                              ": ")
                                      rails-rake-tests-alist
                                      nil nil nil nil
                                      (caar rails-rake-tests-alist))))
  (unless what
    (setq what rails-rake-recent-test-alist))
  (when what
    (let ((task (cdr (assoc what rails-rake-tests-alist))))
      (setq rails-rake-recent-test-alist what)
      (make-local-variable 'compilation-error-regexp-alist)
      (setq compilation-error-regexp-alist rails-rake-test-error-regexp-alist)
      (save-excursion
        (setq default-directory (rails-core:root))
        (compile (format "rake %s" task))))))

(provide 'rails-scripts)