;;; rails-ui.el --- emacs-rails user interface

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,
;;          Rezikov Peter <crazypit13 (at) gmail.com>

;; Keywords: ruby rails languages oop
;; $URL: svn://rubyforge.org/var/svn/emacs-rails/trunk/rails-ui.el $
;; $Id: rails-ui.el 213 2007-08-24 12:57:59Z crazypit $

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

(defconst rails-minor-mode-log-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([test]      '("test.log"         . rails-log:open-test))
      ([pro]       '("production.log"   . rails-log:open-production))
      ([dev]       '("development.log"  . rails-log:open-development))
      ([separator] '("---"))
      ([open]      '("Open Log File..." . rails-log:open)))
    map))

(defconst rails-minor-mode-config-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([routes]      '("routes.rb" .
                       (lambda () (interactive)
                         (rails-core:find-file "config/routes.rb"))))
      ([environment] '("environment.rb" .
                       (lambda() (interactive)
                         (rails-core:find-file "config/environment.rb"))))
      ([database]    '("database.yml" .
                       (lambda() (interactive)
                         (rails-core:find-file "config/database.yml"))))
      ([boot]        '("boot.rb" .
                       (lambda() (interactive)
                         (rails-core:find-file "config/boot.rb"))))
      ([env] (cons "environments" (make-sparse-keymap "environments")))
      ([env test]        '("test.rb" .
                           (lambda() (interactive)
                             (rails-core:find-file "config/environments/test.rb"))))
      ([env production]  '("production.rb" .
                           (lambda() (interactive)
                             (rails-core:find-file "config/environments/production.rb"))))
      ([env development] '("development.rb" .
                           (lambda()(interactive)
                             (rails-core:find-file "config/environments/development.rb")))))
    map))

(defconst rails-minor-mode-nav-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([goto-fixtures]    '("Go to Fixtures"         . rails-nav:goto-fixtures))
      ([goto-plugins]     '("Go to Plugins"          . rails-nav:goto-plugins))
      ([goto-migrate]     '("Go to Migrations"       . rails-nav:goto-migrate))
      ([goto-layouts]     '("Go to Layouts"          . rails-nav:goto-layouts))
      ([goto-stylesheets] '("Go to Stylesheets"      . rails-nav:goto-stylesheets))
      ([goto-javascripts] '("Go to Javascripts"      . rails-nav:goto-javascripts))
      ([goto-helpers]     '("Go to Helpers"          . rails-nav:goto-helpers))
      ([goto-mailers]     '("Go to Mailers"          . rails-nav:goto-mailers))
      ([goto-observers]   '("Go to Observers"        . rails-nav:goto-observers))
      ([goto-unit-tests]  '("Go to Unit Tests"       . rails-nav:goto-unit-tests))
      ([goto-func-tests]  '("Go to Functional Tests" . rails-nav:goto-functional-tests))
      ([goto-models]      '("Go to Models"           . rails-nav:goto-models))
      ([goto-controllers] '("Go to Controllers"      . rails-nav:goto-controllers)))
    map))

(defconst rails-minor-mode-tests-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([integration] '("Integration Tests" . rails-test:run-integration))
      ([unit]        '("Unit Tests"        . rails-test:run-units))
      ([functional]  '("Functional Tests"  . rails-test:run-functionals))
      ([recent]      '("Recent Tests"      . rails-test:run-recent))
      ([tests]       '("All"               . rails-test:run-all))
      ([separator]   '("--"))
      ([toggle]      '(menu-item "Toggle Output Window" rails-script:toggle-output-window
                                 :enable (get-buffer rails-script:buffer-name)))
      ([run-current] '("Test Current Model/Controller/Mailer" . rails-test:run-current))
      ([run]         '("Run Tests ..."                        . rails-test:run)))
    map))

(defconst rails-minor-mode-db-menu-bar-map
  (let ((map (make-sparse-keymap)))
    (define-keys map
      ([clone-db]    '("Clone Development DB to Test DB" . rails-rake:clone-development-db-to-test-db))
      ([load-schema] '("Load schema.rb to DB"               . (lambda() (interactive) (rails-rake:task "db:schema:load"))))
      ([dump-schema] '("Dump DB to schema.rb"               . (lambda() (interactive) (rails-rake:task "db:schema:dump"))))
      ([sep]         '("--"))
      ([prev]        '("Migrate to Previous Version" . rails-rake:migrate-to-prev-version))
      ([version]     '("Migrate to Version ..."      . rails-rake:migrate-to-version))
      ([migrate]     '("Migrate"                     . rails-rake:migrate)))
    map))

(define-keys rails-minor-mode-menu-bar-map

  ([rails] (cons "RoR" (make-sparse-keymap "RubyOnRails")))

  ([rails rails-customize] '("Customize" . (lambda () (interactive) (customize-group 'rails))))
  ([rails separator0] '("--"))
  ([rails svn-status]        '("SVN Status"             . rails-svn-status-into-root))
  ([rails api-doc]           '("Rails API Doc at Point" . rails-browse-api-at-point))
  ([rails sql]               '("SQL Rails Buffer"       . rails-run-sql))
  ([rails tag]               '("Update TAGS File"       . rails-create-tags))
  ([rails ri]                '("Search Documentation"   . rails-search-doc))
  ([rails goto-file-by-line] '("Go to File by Line"     . rails-goto-file-on-current-line))
  ([rails switch-file-menu]  '("Switch file Menu..."    . rails-lib:run-secondary-switch))
  ([rails switch-file]       '("Switch File"            . rails-lib:run-primary-switch))
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
  ([rails scr destr run]        '("Run Destroy ..." . rails-script:destroy))

  ([rails scr gen resource]   '("Resource"         . rails-script:generate-resource))
  ([rails scr gen observer]   '("Observer"         . rails-script:generate-observer))
  ([rails scr gen mailer]     '("Mailer"           . rails-script:generate-mailer))
  ([rails scr gen plugin]     '("Plugin"           . rails-script:generate-plugin))
  ([rails scr gen migration]  '("Migration"        . rails-script:generate-migration))
  ([rails scr gen scaffold]   '("Scaffold"         . rails-script:generate-scaffold))
  ([rails scr gen model]      '("Model"            . rails-script:generate-model))
  ([rails scr gen controller] '("Controller"       . rails-script:generate-controller))
  ([rails scr gen separator]  '("--"))
  ([rails scr gen run]        '("Run Generate ..." . rails-script:generate))

  ([rails scr break]   '("Breakpointer"         . rails-script:breakpointer))
  ([rails scr console] '("Console"              . rails-script:console))
  ([rails scr rake]    '("Rake..."              . rails-rake:task))

  ([rails nav]    (cons "Navigation"    rails-minor-mode-nav-menu-bar-map))
  ([rails config] (cons "Configuration" rails-minor-mode-config-menu-bar-map))
  ([rails log]    (cons "Log Files"     rails-minor-mode-log-menu-bar-map))

  ([rails ws] (cons "WebServer" (make-sparse-keymap "WebServer")))

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

  ([rails ws brows]      '(menu-item "Open Browser..." rails-ws:open-browser-on-controller
                                     :enable (rails-ws:running-p)))
  ([rails ws auto-brows] '(menu-item "Open Browser on Current Action" rails-ws:auto-open-browser
                                     :enable (rails-ws:running-p)))
  ([rails ws url]        '(menu-item "Open Browser" rails-ws:open-browser
                                     :enable (rails-ws:running-p)))
  ([rails ws separator2] '("--"))

  ([rails ws test]        '(menu-item "Start Test" rails-ws:start-test
                                      :enable (not (rails-ws:running-p))))
  ([rails ws production]  '(menu-item "Start Production" rails-ws:start-production
                                      :enable (not (rails-ws:running-p))))
  ([rails ws development] '(menu-item "Start Development" rails-ws:start-development
                                      :enable (not (rails-ws:running-p))))
  ([rails ws separator3] '("--"))

  ([rails ws status]  '(menu-item "Print Status"                                     rails-ws:print-status))
  ([rails ws default] '(menu-item "Start/Stop Web Server (With Default Environment)" rails-ws:toggle-start-stop))
  )

(defcustom rails-minor-mode-prefix-key "\C-c"
  "Key prefix for rails minor mode."
  :group 'rails)

(defmacro rails-key (key)
  `(kbd ,(concat rails-minor-mode-prefix-key " " key)))

(defconst rails-minor-mode-test-current-method-key (rails-key "\C-c ,"))

(defvar rails-minor-mode-map
  (let ((map (make-keymap)))
    map))

(define-keys rails-minor-mode-map
  ([menu-bar] rails-minor-mode-menu-bar-map)
  ([menu-bar rails-tests] (cons "Tests" rails-minor-mode-tests-menu-bar-map))
  ([menu-bar rails-db] (cons "Database" rails-minor-mode-db-menu-bar-map))

  ;; Goto
  ((rails-key "\C-c g m") 'rails-nav:goto-models)
  ((rails-key "\C-c g c") 'rails-nav:goto-controllers)
  ((rails-key "\C-c g o") 'rails-nav:goto-observers)
  ((rails-key "\C-c g n") 'rails-nav:goto-mailers)
  ((rails-key "\C-c g h") 'rails-nav:goto-helpers)
  ((rails-key "\C-c g l") 'rails-nav:goto-layouts)
  ((rails-key "\C-c g s") 'rails-nav:goto-stylesheets)
  ((rails-key "\C-c g j") 'rails-nav:goto-javascripts)
  ((rails-key "\C-c g g") 'rails-nav:goto-migrate)
  ((rails-key "\C-c g p") 'rails-nav:goto-plugins)
  ((rails-key "\C-c g x") 'rails-nav:goto-fixtures)
  ((rails-key "\C-c g f") 'rails-nav:goto-functional-tests)
  ((rails-key "\C-c g u") 'rails-nav:goto-unit-tests)

  ;; Switch
  ((kbd "<M-S-up>")      'rails-lib:run-primary-switch)
  ((kbd "<M-S-down>")    'rails-lib:run-secondary-switch)
  ((rails-key "<up>")     'rails-lib:run-primary-switch)
  ((rails-key "<down>")   'rails-lib:run-secondary-switch)
  ((kbd "<C-return>")    'rails-goto-file-on-current-line)

  ;; Scripts & SQL
  ((rails-key "\C-c e")   'rails-script:generate)
  ((rails-key "\C-c x")   'rails-script:destroy)
  ((rails-key "\C-c s c") 'rails-script:console)
  ((rails-key "\C-c s b") 'rails-script:breakpointer)
  ((rails-key "\C-c s s") 'rails-run-sql)
  ((rails-key "\C-c w s") 'rails-ws:toggle-start-stop)
  ((rails-key "\C-c w d") 'rails-ws:start-development)
  ((rails-key "\C-c w p") 'rails-ws:start-production)
  ((rails-key "\C-c w t") 'rails-ws:start-test)
  ((rails-key "\C-c w i") 'rails-ws:print-status)
  ((rails-key "\C-c w a") 'rails-ws:auto-open-browser)

  ;; Rails finds
  ((rails-key "\C-c f m") 'rails-find:models)
  ((rails-key "\C-c f c") 'rails-find:controller)
  ((rails-key "\C-c f h") 'rails-find:helpers)
  ((rails-key "\C-c f l") 'rails-find:layout)
  ((rails-key "\C-c f s") 'rails-find:stylesheets)
  ((rails-key "\C-c f j") 'rails-find:javascripts)
  ((rails-key "\C-c f g") 'rails-find:migrate)
  ((rails-key "\C-c f b") 'rails-find:lib)
  ((rails-key "\C-c f t") 'rails-find:tasks)
  ((rails-key "\C-c f v") 'rails-find:view)
  ((rails-key "\C-c f d") 'rails-find:db)
  ((rails-key "\C-c f p") 'rails-find:public)
  ((rails-key "\C-c f f") 'rails-find:fixtures)
  ((rails-key "\C-c f o") 'rails-find:config)
  ;; Spec finds
  ((rails-key "\C-c f r s") 'rails-find:spec)
  ((rails-key "\C-c f r c") 'rails-find:spec-controllers)
  ((rails-key "\C-c f r m") 'rails-find:spec-models)
  ((rails-key "\C-c f r h") 'rails-find:spec-helpers)
  ((rails-key "\C-c f r v") 'rails-find:spec-views)
  ((rails-key "\C-c f r f") 'rails-find:spec-fixtures)

  ((rails-key "\C-c d m") 'rails-rake:migrate)
  ((rails-key "\C-c d v") 'rails-rake:migrate-to-version)
  ((rails-key "\C-c d p") 'rails-rake:migrate-to-prev-version)
  ((rails-key "\C-c d t") 'rails-rake:clone-development-db-to-test-db)



  ;; Tests
  ((rails-key "\C-c r")   'rails-rake:task)
  ((rails-key "\C-c t")   'rails-test:run)
  ((rails-key "\C-c .")   'rails-test:run-current)
  ((rails-key "\C-c y i")   'rails-test:run-integration)
  ((rails-key "\C-c y u")   'rails-test:run-units)
  ((rails-key "\C-c y f")   'rails-test:run-functionals)
  ((rails-key "\C-c #")   'rails-test:run-recent)
  ((rails-key "\C-c y a")   'rails-test:run-all)

  ;; Log files
  ((rails-key "\C-c l")    'rails-log:open)
  ((rails-key "\C-c o t")    'rails-log:open-test)
  ((rails-key "\C-c o p")    'rails-log:open-production)
  ((rails-key "\C-c o d")    'rails-log:open-development)

  ;; Tags
  ((rails-key "\C-c \C-t") 'rails-create-tags)

  ;; Documentation
  ([f1]                  'rails-search-doc)
  ((kbd "<C-f1>")        'rails-browse-api-at-point)
  ((rails-key "<f1>")     'rails-browse-api)
  ((rails-key "/")        'rails-script:toggle-output-window)

  ([f9]                  'rails-svn-status-into-root))

;; Global keys and menubar

(global-set-key (rails-key "\C-c j") 'rails-script:create-project)

(when-bind (map (lookup-key global-map  [menu-bar file]))
  (define-key-after
    map
    [create-rails-project]
    '("Create Rails Project" . rails-script:create-project) 'insert-file))

(provide 'rails-ui)
