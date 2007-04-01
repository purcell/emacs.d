;;; rails-ui.el --- emacs-rails user interface

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-ui.el $
;; $Id: rails-ui.el 153 2007-03-31 20:30:51Z dimaexe $

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


;;;;;;;;;; Some init code ;;;;;;;;;;

(defconst rails-minor-mode-nav-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([log] (cons "Open log files" (make-sparse-keymap "Open log files")))
      ([log test]      '("test.log"         . rails-log:open-test))
      ([log pro]       '("production.log"   . rails-log:open-production))
      ([log dev]       '("development.log"  . rails-log:open-development))
      ([log separator] '("---"))
      ([log open]      '("Open log file..." . rails-log:open))

      ([config] (cons "Configuration" (make-sparse-keymap "Configuration")))
      ([config routes]      '("routes.rb" .
                              (lambda () (interactive)
                                (rails-core:find-file "config/routes.rb"))))
      ([config environment] '("environment.rb" .
                              (lambda() (interactive)
                                (rails-core:find-file "config/environment.rb"))))
      ([config database]    '("database.yml" .
                              (lambda() (interactive)
                                (rails-core:find-file "config/database.yml"))))
      ([config boot]        '("boot.rb" .
                              (lambda() (interactive)
                                (rails-core:find-file "config/boot.rb"))))
      ([config env] (cons "environments" (make-sparse-keymap "environments")))
      ([config env test]        '("test.rb" .
                                  (lambda() (interactive)
                                    (rails-core:find-file "config/environments/test.rb"))))
      ([config env production]  '("production.rb" .
                                  (lambda() (interactive)
                                    (rails-core:find-file "config/environments/production.rb"))))
      ([config env development] '("development.rb" .
                                  (lambda()(interactive)
                                    (rails-core:find-file "config/environments/development.rb"))))

      ([separator]        '("---"))
      ([goto-fixtures]    '("Go to fixtures"         . rails-nav:goto-fixtures))
      ([goto-plugins]     '("Go to plugins"          . rails-nav:goto-plugins))
      ([goto-migrate]     '("Go to migrations"       . rails-nav:goto-migrate))
      ([goto-layouts]     '("Go to layouts"          . rails-nav:goto-layouts))
      ([goto-stylesheets] '("Go to stylesheets"      . rails-nav:goto-stylesheets))
      ([goto-javascripts] '("Go to javascripts"      . rails-nav:goto-javascripts))
      ([goto-helpers]     '("Go to helpers"          . rails-nav:goto-helpers))
      ([goto-mailers]     '("Go to mailers"          . rails-nav:goto-mailers))
      ([goto-observers]   '("Go to observers"        . rails-nav:goto-observers))
      ([goto-unit-tests]  '("Go to unit tests"       . rails-nav:goto-unit-tests))
      ([goto-func-tests]  '("Go to functional tests" . rails-nav:goto-functional-tests))
      ([goto-models]      '("Go to models"           . rails-nav:goto-models))
      ([goto-controllers] '("Go to controllers"      . rails-nav:goto-controllers)))
    map))

(defconst rails-minor-mode-tests-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([integration] '("Integration tests" . (lambda() (interactive) (rails-test:run "integration"))))
      ([unit]        '("Unit tests"        . (lambda() (interactive) (rails-test:run "units"))))
      ([functional]  '("Functional tests"  . (lambda() (interactive) (rails-test:run "functionals"))))
      ([recent]      '("Recent tests"      . (lambda() (interactive) (rails-test:run "recent"))))
      ([tests]       '("All"               . (lambda() (interactive) (rails-test:run "all"))))
      ([separator]   '("--"))
      ([method]      '(menu-item "Test current method" rails-test:run-current-method
                                 :enable (find (rails-core:buffer-type) '(:unit-test :functional-test))))
      ([toggle]      '(menu-item "Toggle output window" rails-script:toggle-output-window
                                 :enable (get-buffer rails-script:buffer-name)))
      ([run-current] '("Test current model/controller/mailer" . rails-test:run-current))
      ([run]         '("Run tests ..."                        . rails-test:run)))
    map))

(defconst rails-minor-mode-db-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([migrate] '("Migrate"                     . rails-rake:migrate))
      ([version] '("Migrate to version ..."      . rails-rake:migrate-to-version))
      ([prev]    '("Migrate to previous version" . rails-rake:migrate-to-prev-version)))
    map))

(define-keys rails-minor-mode-menu-bar-map

  ([rails] (cons "RubyOnRails" (make-sparse-keymap "RubyOnRails")))

  ([rails rails-customize] '("Customize" . (lambda () (interactive) (customize-group 'rails))))
  ([rails separator0] '("--"))
  ([rails svn-status] '("SVN status" . rails-svn-status-into-root))
  ([rails api-doc]           '("Rails API doc at point" . rails-browse-api-at-point))
  ([rails sql]               '("SQL Rails buffer"       . rails-run-sql))
  ([rails tag]               '("Update TAGS file"       . rails-create-tags))
  ([rails ri]                '("Search documentation"   . rails-search-doc))
  ([rails goto-file-by-line] '("Goto file by line"      . rails-goto-file-on-current-line))
  ([rails switch-file-menu]  '("Switch file menu..."    . rails-lib:run-secondary-switch))
  ([rails switch-file]       '("Switch file"            . rails-lib:run-primary-switch))
  ([rails separator1]        '("--"))

  ([rails scr] (cons "Scripts" (make-sparse-keymap "Scripts")))

  ([rails scr gen] (cons "Generate" (make-sparse-keymap "Generate")))
  ([rails scr destr] (cons "Destroy" (make-sparse-keymap "Generators")))

  ([rails scr destr resource]   '("Resource"        . rails-script:destroy-resource))
  ([rails scr destr observer]   '("Observer"        . rails-script:destroy-observer))
  ([rails scr destr mailer]     '("Mailer"          . rails-script:destroy-mailer))
  ([rails scr destr plugin]     '("Plugin"          . rails-script:destroy-plugin))
  ([rails scr destr migration]  '("Migration"       . rails-script:destroy-migration))
  ([rails scr destr scaffold]   '("Scaffold"        . rails-script:destroy-scaffold))
  ([rails scr destr model]      '("Model"           . rails-script:destroy-model))
  ([rails scr destr controller] '("Controller"      . rails-script:destroy-controller))
  ([rails scr destr separator]  '("--"))
  ([rails scr destr run]        '("Run destroy ..." . rails-script:destroy))

  ([rails scr gen resource]   '("Resource"         . rails-script:generate-resource))
  ([rails scr gen observer]   '("Observer"         . rails-script:generate-observer))
  ([rails scr gen mailer]     '("Mailer"           . rails-script:generate-mailer))
  ([rails scr gen plugin]     '("Plugin"           . rails-script:generate-plugin))
  ([rails scr gen migration]  '("Migration"        . rails-script:generate-migration))
  ([rails scr gen scaffold]   '("Scaffold"         . rails-script:generate-scaffold))
  ([rails scr gen model]      '("Model"            . rails-script:generate-model))
  ([rails scr gen controller] '("Controller"       . rails-script:generate-controller))
  ([rails scr gen separator]  '("--"))
  ([rails scr gen run]        '("Run generate ..." . rails-script:generate))

  ([rails scr break]   '("Breakpointer"         . rails-script:breakpointer))
  ([rails scr console] '("Console"              . rails-script:console))
  ([rails scr rake]    '("Rake..."              . rails-rake:task))

  ([rails ws] (cons "Web Server" (make-sparse-keymap "WebServer")))

  ([rails ws use-webrick]  '(menu-item "Use WEBrick" (lambda() (interactive)
                                                       (rails-ws:switch-default-server-type "webrick"))
                                       :button (:toggle . (rails-ws:default-server-type-p "webrick"))))
  ([rails ws use-lighttpd] '(menu-item "Use Lighty" (lambda() (interactive)
                                                      (rails-ws:switch-default-server-type "lighttpd"))
                                       :button (:toggle . (rails-ws:default-server-type-p "lighttpd"))))
  ([rails ws use-mongrel]  '(menu-item "Use Mongrel" (lambda() (interactive)
                                                       (rails-ws:switch-default-server-type "mongrel"))
                                       :button (:toggle . (rails-ws:default-server-type-p "mongrel"))))
  ([rails ws separator] '("--"))

  ([rails ws brows]      '(menu-item "Open browser..." rails-ws:open-browser-on-controller
                                     :enable (rails-ws:running-p)))
  ([rails ws auto-brows] '(menu-item "Open browser on current action" rails-ws:auto-open-browser
                                     :enable (rails-ws:running-p)))
  ([rails ws url]        '(menu-item "Open browser" rails-ws:open-browser
                                     :enable (rails-ws:running-p)))
  ([rails ws separator2] '("--"))

  ([rails ws test]        '(menu-item "Start test" rails-ws:start-test
                                      :enable (not (rails-ws:running-p))))
  ([rails ws production]  '(menu-item "Start production" rails-ws:start-production
                                      :enable (not (rails-ws:running-p))))
  ([rails ws development] '(menu-item "Start development" rails-ws:start-development
                                      :enable (not (rails-ws:running-p))))
  ([rails ws separator3] '("--"))

  ([rails ws status]  '(menu-item "Print status"                                     rails-ws:print-status))
  ([rails ws default] '(menu-item "Start/stop web server (with default environment)" rails-ws:toggle-start-stop))
  )

(defvar rails-minor-mode-map (make-sparse-keymap))

(define-keys rails-minor-mode-map
  ([menu-bar] rails-minor-mode-menu-bar-map)
  ([menu-bar rails-tests] (cons "Tests" rails-minor-mode-tests-menu-bar-map))
  ([menu-bar snippets] (cons "Snippets" (rails-snippets:create-keymap)))
  ([menu-bar rails-db] (cons "Database" rails-minor-mode-db-menu-bar-map))
  ([menu-bar rails-nav] (cons "Navigate" rails-minor-mode-nav-menu-bar-map))

  ;; Goto
  ((kbd "\C-c \C-c g m") 'rails-nav:goto-models)
  ((kbd "\C-c \C-c g c") 'rails-nav:goto-controllers)
  ((kbd "\C-c \C-c g o") 'rails-nav:goto-observers)
  ((kbd "\C-c \C-c g n") 'rails-nav:goto-mailers)
  ((kbd "\C-c \C-c g h") 'rails-nav:goto-helpers)
  ((kbd "\C-c \C-c g l") 'rails-nav:goto-layouts)
  ((kbd "\C-c \C-c g s") 'rails-nav:goto-stylesheets)
  ((kbd "\C-c \C-c g j") 'rails-nav:goto-javascripts)
  ((kbd "\C-c \C-c g g") 'rails-nav:goto-migrate)
  ((kbd "\C-c \C-c g p") 'rails-nav:goto-plugins)
  ((kbd "\C-c \C-c g x") 'rails-nav:goto-fixtures)
  ((kbd "\C-c \C-c g f") 'rails-nav:goto-functional-tests)
  ((kbd "\C-c \C-c g u") 'rails-nav:goto-unit-tests)

  ;; Switch
  ((kbd "\C-c <up>")     'rails-lib:run-primary-switch)
  ((kbd "\C-c <down>")   'rails-lib:run-secondary-switch)
  ((kbd "<M-S-up>")      'rails-lib:run-primary-switch)
  ((kbd "<M-S-down>")    'rails-lib:run-secondary-switch)
  ((kbd "<C-return>")    'rails-goto-file-on-current-line)

  ;; Scripts & SQL
  ((kbd "\C-c \C-c e")   'rails-script:generate)
  ((kbd "\C-c \C-c x")   'rails-script:destroy)
  ((kbd "\C-c \C-c s c") 'rails-script:console)
  ((kbd "\C-c \C-c s b") 'rails-script:breakpointer)
  ((kbd "\C-c \C-c s s") 'rails-run-sql)
  ((kbd "\C-c \C-c w s") 'rails-ws:toggle-start-stop)
  ((kbd "\C-c \C-c w d") 'rails-ws:start-development)
  ((kbd "\C-c \C-c w p") 'rails-ws:start-production)
  ((kbd "\C-c \C-c w t") 'rails-ws:start-test)
  ((kbd "\C-c \C-c w i") 'rails-ws:print-status)
  ((kbd "\C-c \C-c w a") 'rails-ws:auto-open-browser)

  ;; Rails finds
  ((kbd "\C-c \C-c f m") 'rails-find:models)
  ((kbd "\C-c \C-c f c") 'rails-find:controller)
  ((kbd "\C-c \C-c f h") 'rails-find:helpers)
  ((kbd "\C-c \C-c f l") 'rails-find:layout)
  ((kbd "\C-c \C-c f s") 'rails-find:stylesheets)
  ((kbd "\C-c \C-c f j") 'rails-find:javascripts)
  ((kbd "\C-c \C-c f g") 'rails-find:migrate)
  ((kbd "\C-c \C-c f b") 'rails-find:lib)
  ((kbd "\C-c \C-c f t") 'rails-find:tasks)
  ((kbd "\C-c \C-c f v") 'rails-find:view)
  ((kbd "\C-c \C-c f d") 'rails-find:db)
  ((kbd "\C-c \C-c f p") 'rails-find:public)
  ((kbd "\C-c \C-c f f") 'rails-find:fixtures)
  ((kbd "\C-c \C-c f o") 'rails-find:config)

  ((kbd "\C-c \C-c d m") 'rails-rake:migrate)
  ((kbd "\C-c \C-c d v") 'rails-rake:migrate-to-version)
  ((kbd "\C-c \C-c d p") 'rails-rake:migrate-to-prev-version)

  ;; Tests
  ((kbd "\C-c \C-c r")   'rails-rake:task)
  ((kbd "\C-c \C-c t")   'rails-test:run)
  ((kbd "\C-c \C-c .")   'rails-test:run-current)

  ;; Navigation

  ((kbd "\C-c \C-c l")    'rails-log:open)
  ;; Tags
  ((kbd "\C-c \C-c \C-t") 'rails-create-tags)

  ;; Documentation
  ([f1]                  'rails-search-doc)
  ((kbd "<C-f1>")        'rails-browse-api-at-point)
  ((kbd "\C-c <f1>")     'rails-browse-api)
  ((kbd "\C-c /")        'rails-script:toggle-output-window)

  ([f9]                  'rails-svn-status-into-root))

;; Global keys and menubar

(global-set-key (kbd "\C-c \C-c j") 'rails-script:create-project)

(when-bind (map (lookup-key global-map  [menu-bar file]))
  (define-key-after
    map
    [create-rails-project]
    '("Create Rails Project" . rails-script:create-project) 'insert-file))

(provide 'rails-ui)
