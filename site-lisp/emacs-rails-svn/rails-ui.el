;;; rails-ui.el --- emacs-rails user interface

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


;;;;;;;;;; Some init code ;;;;;;;;;;

(unless (boundp 'html-mode-abbrev-table)
  (setq html-mode-abbrev-table (make-abbrev-table)))
(unless (boundp 'html-helper-mode-abbrev-table)
  (setq html-helper-mode-abbrev-table (make-abbrev-table)))
(unless (boundp 'nxml-mode-abbrev-table)
  (setq nxml-mode-abbrev-table (make-abbrev-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rails snips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-snips (ruby-mode-abbrev-table)
  ("ra"      "render :action => \"$${action}\""
             "render (action)")
  ("ral"     "render :action => \"$${action}\", :layout => \"$${layoutname}\""
             "render (action,layout)")
  ("rf"      "render :file => \"$${filepath}\""
             "render (file)")
  ("rfu"     "render :file => \"$${filepath}\", :use_full_path => $${false}"
             "render (file,use_full_path)")
  ("ri"      "render :inline => \"$${<%= 'hello' %>}\""
             "render (inline)")
  ("ril"     "render :inline => \"$${<%= 'hello' %>}\", :locals => { $${name} => \"$${value}\" }"
             "render (inline,locals)")
  ("rit"     "render :inline => \"$${<%= 'hello' %>}\", :type => :$${rxml})"
             "render (inline,type)")
  ("rl"      "render :layout => \"$${layoutname}\""
             "render (layout)")
  ("rn"      "render :nothing => $${true}"
             "render (nothing)")
  ("rns"     "render :nothing => $${true}, :status => $${401}"
             "render (nothing,status)")
  ("rp"      "render :partial => \"$${item}\""
             "render (partial)")
  ("rpc"     "render :partial => \"$${item}\", :collection => $${items}"
             "render (partial,collection)")
  ("rpl"     "render :partial => \"$${item}\", :locals => { :$${name} => \"$${value}\"}"
             "render (partial,locals)")
  ("rpo"     "render :partial => \"$${item}\", :object => $${object}"
             "render (partial,object)")
  ("rps"     "render :partial => \"$${item}\", :status => $${500}"
             "render (partial,status)")
  ("rt"      "render :text => \"$${Text here...}\""
             "render (text)")
  ("rtl"     "render :text => \"$${Text here...}\", :layout => \"$${layoutname}\""
             "render (text,layout)")
  ("rtlt"    "render :text => \"$${Text here...}\", :layout => $${true}"
             "render (text,layout => true)")
  ("rts"     "render :text => \"$${Text here...}\", :status => $${401}"
             "")
  ("rcea"    "render_component :action => \"$${index}\""
             "render_component (action)")
  ("rcec"    "render_component :controller => \"$${items}\""
             "render_component (controller)")
  ("rceca"   "render_component :controller => \"$${items}\", :action => \"$${index}\""
             "render_component (controller, action)")
  ("rea"     "redirect_to :action => \"$${index}\""
             "redirect_to (action)")
  ("reai"    "redirect_to :action => \"$${show}\", :id => $${@item}"
             "redirect_to (action, id)")
  ("rec"     "redirect_to :controller => \"$${items}\""
             "redirect_to (controller)")
  ("reca"    "redirect_to :controller => \"$${items}\", :action => \"$${list}\""
             "redirect_to (controller, action)")
  ("recai"   "redirect_to :controller => \"$${items}\", :action => \"$${show}\", :id => $${@item}"
             "redirect_to (controller, action, id)")

  ;; Environment
  ("flash"   "flash[:$${notice}] = \"$${Text here...}\""
             "flash[...]")
  ("logi"    "logger.info \"$${Text here...}\""
             "logger.info")
  ("par"     "params[:$${id}]"
             "params[...]")
  ("ses"     "session[:$${user}]"
             "session[...]")

  ;; Models
  ("bt"      "belongs_to :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\""
             "belongs_to (class_name,foreign_key)")
  ("hm"      "has_many :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"
             "has_many (class_name,foreign_key,dependent)")
  ("ho"      "has_one :$${model}, :class_name => \"$${class}\", :foreign_key => \"$${key}\", :dependent => :$${destroy}"
             "has_one (class_name,foreign_key,dependent)")
  ("vp"      "validates_presence_of :$${attr}"
             "validates_presence_of")
  ("vu"      "validates_uniqueness_of :$${attr}"
             "validates_uniqueness_of")
  ("vn"     "validates_numericality_of :$${attr}"
             "validates_numericality_of")

  ;; Migrations
  ("mct"     "create_table :$${name} do |t|\n$>$.\nend"
             "create_table (table)")
  ("mctf"    "create_table :$${name}, :force => true do |t|\n$>$.\nend"
             "create_table (table, force)")
  ("mdt"     "drop_table :$${name}"
             "drop_table (table)")
  ("mtcl"    "t.column \"$${name}\", :$${type}"
             "t.column (column)")
  ("mac"     "add_column :$${table_name}, :$${column_name}, :$${type}"
             "add_column (table, column, type)")
  ("mcc"     "change_column :$${table_name}, :$${column_name}, :$${type}"
             "change_column (table, column, type)")
  ("mrec"    "rename_column :$${table_name}, :$${column_name}, :$${new_column_name}"
             "rename_column (table, name, new_name)")
  ("mrmc"    "remove_column :$${table_name}, :$${column_name}"
             "remove_column (table, column)")
  ("mai"     "add_index :$${table_name}, :$${column_name}"
             "add_index (table, column)")
  ("mait"    "add_index :$${table_name}, :$${column_name}, :$${index_type}"
             "add_index (table, column, type)")
  ("mrmi"    "remove_index :$${table_name}, :$${column_name}"
             "remove_index (table, column)"))

;;;; ERB Snips ;;;;

(def-snips (html-mode-abbrev-table html-helper-mode-abbrev-table nxml-mode-abbrev-table)
  ("ft"      "<%= form_tag :action => \"$${update}\" %>\n$.\n<%= end_form_tag %>"
             "form_tag")
  ("lia"     "<%= link_to \"$${title}\", :action => \"$${index}\" %>"
             "link_to (action)")
  ("liai"    "<%= link_to \"$${title}\", :action => \"$${edit}\", :id => $${@item} %>"
             "link_to (action, id)")
  ("lic"     "<%= link_to \"$${title}\", :controller => \"$${items}\" %>"
             "link_to (controller)")
  ("lica"    "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${index}\" %>"
             "link_to (controller, action)")
  ("licai"   "<%= link_to \"$${title}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${@item} %>"
             "link_to (controller, action, id)")
  ("%h"      "<%=h $${@item} %>"
             "<% h ... %>")
  ("%if"     "<% if $${cond} -%>\n$.\n<% end -%>"
             "<% if/end %>")
  ("%ifel"   "<% if $${cond} -%>\n$.\n<% else -%>\n<% end -%>"
             "<% if/else/end %>")
  ("%unless" "<% unless $${cond} -%>\n$.\n<% end -%>"
             "<% unless/end %>")
  ("%for" "<% for $${elem} in @$${list} %>\n$>$.\n<% end %>$>"
             "<% for/end %>")

  ("%"       "<% $. -%>"
             "<% ... %>")
  ("%%"      "<%= $. %>"
             "<%= ... %>"))

(define-keys rails-minor-mode-menu-bar-map
    ([rails] (cons "RubyOnRails" (make-sparse-keymap "RubyOnRails")))
    ([rails svn-status]
      '(menu-item "SVN status" rails-svn-status-into-root
                  :enable (rails-core:root)))

    ([rails api-doc] '("Rails API doc at point" . rails-browse-api-at-point))

    ([rails sql] '("SQL Rails buffer" . rails-run-sql))

    ([rails tag] '("Update TAGS file" . rails-create-tags))
    ([rails ri] '("Search documentation" . rails-search-doc))
    ([rails goto-file-by-line]
      '("Goto file by line" . rails-goto-file-on-current-line))
    ([rails switch-file-menu]
      '("Switch file menu..." . rails-goto-file-from-file-with-menu))
    ([rails switch-file]
      '("Switch file" . rails-goto-file-from-file))
    ([rails separator] '("--"))

    ([rails snip] (cons "Snippets" (make-sparse-keymap "Snippets")))
    ([rails snip render] (cons "render" (make-sparse-keymap "render")))
    ([rails snip render sk-ra]  (snippet-menu-line 'ruby-mode-abbrev-table "ra"))
    ([rails snip render sk-ral] (snippet-menu-line 'ruby-mode-abbrev-table "ral"))
    ([rails snip render sk-rf]  (snippet-menu-line 'ruby-mode-abbrev-table "rf"))
    ([rails snip render sk-rfu] (snippet-menu-line 'ruby-mode-abbrev-table "rfu"))
    ([rails snip render sk-ri]  (snippet-menu-line 'ruby-mode-abbrev-table "ri"))
    ([rails snip render sk-ril] (snippet-menu-line 'ruby-mode-abbrev-table "ril"))
    ([rails snip render sk-rit] (snippet-menu-line 'ruby-mode-abbrev-table "rit"))
    ([rails snip render sk-rl]  (snippet-menu-line 'ruby-mode-abbrev-table "rl"))
    ([rails snip render sk-rn]  (snippet-menu-line 'ruby-mode-abbrev-table "rn"))
    ([rails snip render sk-rns] (snippet-menu-line 'ruby-mode-abbrev-table "rns"))
    ([rails snip render sk-rp]  (snippet-menu-line 'ruby-mode-abbrev-table "rp"))
    ([rails snip render sk-rpc] (snippet-menu-line 'ruby-mode-abbrev-table "rpc"))
    ([rails snip render sk-rpl] (snippet-menu-line 'ruby-mode-abbrev-table "rpl"))
    ([rails snip render sk-rpo] (snippet-menu-line 'ruby-mode-abbrev-table "rpo"))
    ([rails snip render sk-rps] (snippet-menu-line 'ruby-mode-abbrev-table "rps"))
    ([rails snip render sk-rt] (snippet-menu-line 'ruby-mode-abbrev-table "rt"))
    ([rails snip render sk-rtl] (snippet-menu-line 'ruby-mode-abbrev-table "rtl"))
    ([rails snip render sk-rtlt] (snippet-menu-line 'ruby-mode-abbrev-table "rtlt"))
    ([rails snip render sk-rcea] (snippet-menu-line 'ruby-mode-abbrev-table "rcea"))
    ([rails snip render sk-rcec] (snippet-menu-line 'ruby-mode-abbrev-table "rcec"))
    ([rails snip render sk-rceca] (snippet-menu-line 'ruby-mode-abbrev-table "rceca"))

    ([rails snip redirect_to] (cons "redirect_to" (make-sparse-keymap "redirect_to")))
    ([rails snip redirect_to sk-rea] (snippet-menu-line 'ruby-mode-abbrev-table "rea"))
    ([rails snip redirect_to sk-reai] (snippet-menu-line 'ruby-mode-abbrev-table "reai"))
    ([rails snip redirect_to sk-rec] (snippet-menu-line 'ruby-mode-abbrev-table "rec"))
    ([rails snip redirect_to sk-reca] (snippet-menu-line 'ruby-mode-abbrev-table "reca"))
    ([rails snip redirect_to sk-recai] (snippet-menu-line 'ruby-mode-abbrev-table "recai"))

    ([rails snip environment] (cons "environment" (make-sparse-keymap "environment")))
    ([rails snip environment sk-flash] (snippet-menu-line 'ruby-mode-abbrev-table "flash"))
    ([rails snip environment sk-logi] (snippet-menu-line 'ruby-mode-abbrev-table "logi"))
    ([rails snip environment sk-params] (snippet-menu-line 'ruby-mode-abbrev-table "par"))
    ([rails snip environment sk-session] (snippet-menu-line 'ruby-mode-abbrev-table "ses"))

    ([rails snip model] (cons "model" (make-sparse-keymap "model")))
    ([rails snip model sk-bt] (snippet-menu-line 'ruby-mode-abbrev-table "bt"))
    ([rails snip model sk-hm] (snippet-menu-line 'ruby-mode-abbrev-table "hm"))
    ([rails snip model sk-ho] (snippet-menu-line 'ruby-mode-abbrev-table "ho"))
    ([rails snip model sk-vp] (snippet-menu-line 'ruby-mode-abbrev-table "vp"))
    ([rails snip model sk-vu] (snippet-menu-line 'ruby-mode-abbrev-table "vu"))
    ([rails snip model sk-vn] (snippet-menu-line 'ruby-mode-abbrev-table "vn"))
    ([rails snip migrations] (cons "migrations" (make-sparse-keymap "model")))
    ([rails snip migrations mct] (snippet-menu-line 'ruby-mode-abbrev-table "mct"))
    ([rails snip migrations mctf] (snippet-menu-line 'ruby-mode-abbrev-table "mctf"))
    ([rails snip migrations mdt] (snippet-menu-line 'ruby-mode-abbrev-table "mdt"))
    ([rails snip migrations mtcl] (snippet-menu-line 'ruby-mode-abbrev-table "mtcl"))
    ([rails snip migrations mac] (snippet-menu-line 'ruby-mode-abbrev-table "mac"))
    ([rails snip migrations mcc] (snippet-menu-line 'ruby-mode-abbrev-table "mcc"))
    ([rails snip migrations mrec] (snippet-menu-line 'ruby-mode-abbrev-table "mrec"))
    ([rails snip migrations mrmc] (snippet-menu-line 'ruby-mode-abbrev-table "mrmc"))
    ([rails snip migrations mai] (snippet-menu-line 'ruby-mode-abbrev-table "mai"))
    ([rails snip migrations mait] (snippet-menu-line 'ruby-mode-abbrev-table "mait"))
    ([rails snip migrations mrmi] (snippet-menu-line 'ruby-mode-abbrev-table "mrmi"))

    ([rails snip rhtml] (cons "rhtml" (make-sparse-keymap "rhtml")))
    ([rails snip rhtml sk-erb-ft] (snippet-menu-line 'html-mode-abbrev-table "ft"))
    ([rails snip rhtml sk-erb-lia] (snippet-menu-line 'html-mode-abbrev-table "lia"))
    ([rails snip rhtml sk-erb-liai] (snippet-menu-line 'html-mode-abbrev-table "liai"))
    ([rails snip rhtml sk-erb-lic] (snippet-menu-line 'html-mode-abbrev-table "lic"))
    ([rails snip rhtml sk-erb-lica] (snippet-menu-line 'html-mode-abbrev-table "lica"))
    ([rails snip rhtml sk-erb-licai] (snippet-menu-line 'html-mode-abbrev-table "licai"))
    ([rails snip rhtml sk-erb-h] (snippet-menu-line 'html-mode-abbrev-table "%h"))
    ([rails snip rhtml sk-erb-if] (snippet-menu-line 'html-mode-abbrev-table "%if"))
    ([rails snip rhtml sk-erb-unless] (snippet-menu-line 'html-mode-abbrev-table "%unless"))
    ([rails snip rhtml sk-erb-ifel] (snippet-menu-line 'html-mode-abbrev-table "%ifel"))
    ([rails snip rhtml sk-erb-block] (snippet-menu-line 'html-mode-abbrev-table "%"))
    ([rails snip rhtml sk-erb-echo-block] (snippet-menu-line 'html-mode-abbrev-table "%%"))

    ([rails log] (cons "Open log" (make-sparse-keymap "Open log")))
    ([rails log test]
      '("test.log" . (lambda() (interactive) (rails-open-log "test"))))
    ([rails log pro]
      '("production.log" . (lambda() (interactive) (rails-open-log "production"))))
    ([rails log dev]
      '("development.log" . (lambda() (interactive) (rails-open-log "development"))))

    ([rails config] (cons "Configuration" (make-sparse-keymap "Configuration")))

    ([rails config routes]
     '("routes.rb" . (lambda ()
           (interactive)
           (rails-core:find-file "config/routes.rb"))))
    ([rails config environment]
     '("environment.rb" .
       (lambda()
   (interactive)
   (rails-core:find-file "config/environment.rb"))))
    ([rails config database]
     '("database.yml" .
       (lambda()
   (interactive)
   (rails-core:find-file "config/database.yml"))))
    ([rails config boot]
     '("boot.rb" .
       (lambda()
   (interactive)
   (rails-core:find-file "config/boot.rb"))))

    ([rails config env] (cons "environments" (make-sparse-keymap "environments")))
    ([rails config env test]
     '("test.rb" .
       (lambda()
   (interactive)
   (rails-core:find-file "config/environments/test.rb"))))
    ([rails config env production]
     '("production.rb" .
       (lambda()
   (interactive)
   (rails-core:find-file "config/environments/production.rb"))))
    ([rails config env development]
     '("development.rb" .
       (lambda()
   (interactive)
   (rails-core:find-file "config/environments/development.rb"))))

    ([rails scr] (cons "Scripts" (make-sparse-keymap "Scripts")))

    ([rails scr proj] '("Create project" . rails-create-project))
    ([rails scr rake] '("Rake ..." . rails-rake))
    ([rails scr console] '("Console" . rails-run-console))
    ([rails scr break] '("Breakpointer" . rails-run-breakpointer))


    ([rails scr gen] (cons "Generate" (make-sparse-keymap "Generate")))
    ([rails scr gen migration] '("Migration" . rails-generate-migration))
    ([rails scr gen scaffold] '("Scaffold" . rails-generate-scaffold))
    ([rails scr gen model] '("Model" . rails-generate-model))
    ([rails scr gen controller] '("Controller" . rails-generate-controller))

    ([rails scr destr] (cons "Destroy" (make-sparse-keymap "Generators")))
    ([rails scr destr controller] '("Controller" . rails-destroy-controller))
    ([rails scr destr model] '("Model" . rails-destroy-model))
    ([rails scr destr scaffold] '("Scaffold" . rails-destroy-scaffold))


    ([rails webrick] (cons "WEBrick" (make-sparse-keymap "WEBrick")))

    ([rails webrick mongrel]
      '(menu-item "Use Mongrel" rails-webrick:toggle-use-mongrel
                  :enable (not (rails-webrick:status))
                  :button (:toggle
                           . (and (boundp 'rails-webrick:use-mongrel)
                                  rails-webrick:use-mongrel))))

    ([rails webrick separator] '("--"))

    ([rails webrick brows]
      '(menu-item "Open browser..."
		  rails-webrick:open-browser-on-controller
		  :enable (rails-webrick:status)))

    ([rails webrick auto-brows]
      '(menu-item "Open browser on current action"
		  rails-webrick:auto-open-browser
		  :enable (rails-webrick:status)))
    
    ([rails webrick url]
      '(menu-item "Open browser"
                  rails-webrick:open-browser
                  :enable (rails-webrick:status)))
    ([rails webrick stop]
      '(menu-item "Stop"
                  rails-webrick:stop
                  :enable (rails-webrick:status)))
    ([rails webrick test]
      '(menu-item "Start test"
                  (lambda() (interactive) (rails-webrick:start "test"))
                  :enable (not (rails-webrick:status))))
    ([rails webrick production]
      '(menu-item "Start production"
                  (lambda() (interactive) (rails-webrick:start "production"))
                  :enable (not (rails-webrick:status))))
    ([rails webrick development]
      '(menu-item "Start development"
                  (lambda() (interactive) (rails-webrick:start "development"))
                  :enable (not (rails-webrick:status))))

    ([rails separator2] '("--"))

    ([rails goto-models] '("Goto models" . rails-nav:goto-models))
    ([rails goto-controllers] '("Goto controllers" . rails-nav:goto-controllers))
    ([rails goto-helpers] '("Goto helpers" . rails-nav:goto-helpers))
    ([rails goto-layouts] '("Goto layouts" . rails-nav:goto-layouts))
    ([rails goto-stylesheets] '("Goto stylesheets" . rails-nav:goto-stylesheets))
    ([rails goto-javascripts] '("Goto javascripts" . rails-nav:goto-javascripts))
    ([rails goto-migrate] '("Goto migrate" . rails-nav:goto-migrate)))

(define-keys rails-minor-mode-map
  ([menu-bar] rails-minor-mode-menu-bar-map)
  ;; Goto
  ((kbd "\C-c g m") 'rails-nav:goto-models)
  ((kbd "\C-c g c") 'rails-nav:goto-controllers)
  ((kbd "\C-c g h") 'rails-nav:goto-helpers)
  ((kbd "\C-c g l") 'rails-nav:goto-layouts)
  ((kbd "\C-c g s") 'rails-nav:goto-stylesheets)
  ((kbd "\C-c g j") 'rails-nav:goto-javascripts)
  ((kbd "\C-c g g") 'rails-nav:goto-migrate)

  ;; Switch
  ((kbd "C-c <up>") 'rails-lib:run-primary-switch)
  ((kbd "C-c <down>") 'rails-lib:run-secondary-switch)

  ;; Scripts & SQL
  ((kbd "\C-c s g c") 'rails-generate-controller)
  ((kbd "\C-c s g m") 'rails-generate-model)
  ((kbd "\C-c s g s") 'rails-generate-scaffold)
  ((kbd "\C-c s g g") 'rails-generate-migration)
  ((kbd "\C-c s d c") 'rails-destroy-controller)
  ((kbd "\C-c s d m") 'rails-destroy-model)
  ((kbd "\C-c s d s") 'rails-destroy-scaffold)
  ((kbd "\C-c s c")   'rails-run-console)
  ((kbd "\C-c s b")   'rails-run-breakpointer)
  ((kbd "\C-c s s")   'rails-run-sql)
  ((kbd "\C-c s r")   'rails-rake)
  ((kbd "\C-c s w")   'rails-webrick:start)

  ;; Rails finds
  ((kbd "\C-c f m") 'rails-find-models)
  ((kbd "\C-c f c") 'rails-find-controller)
  ((kbd "\C-c f h") 'rails-find-helpers)
  ((kbd "\C-c f l") 'rails-find-layout)
  ((kbd "\C-c f s") 'rails-find-stylesheets)
  ((kbd "\C-c f j") 'rails-find-javascripts)
  ((kbd "\C-c f g") 'rails-find-migrate)

  ((kbd "\C-c f v") 'rails-find-view)
  ((kbd "\C-c f d") 'rails-find-db)
  ((kbd "\C-c f p") 'rails-find-public)
  ((kbd "\C-c f f") 'rails-find-fixtures)
  ((kbd "\C-c f o") 'rails-find-config)

  ;; Navigation
  ((kbd "<C-return>") 'rails-goto-file-on-current-line)
  ((kbd "<M-S-down>") 'rails-goto-file-from-file-with-menu)
  ((kbd "<M-S-up>")   'rails-goto-file-from-file)
  ((kbd "\C-c l") 'rails-open-log)

  ;; Tags
  ((kbd "\C-c \C-t") 'rails-create-tags)


  ;; Browser
  ((kbd "\C-c <f5>") 'rails-webrick:auto-open-browser)
  ;;; Doc
  ([f1]  'rails-search-doc)
  ((kbd "<C-f1>")  'rails-browse-api-at-point)
  ((kbd "C-c <f1>")  'rails-browse-api)
  
  ([f9]  'rails-svn-status-into-root))


(provide 'rails-ui)