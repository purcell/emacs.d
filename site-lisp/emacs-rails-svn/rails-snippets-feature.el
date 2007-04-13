;;; rails-snippets-feature.el --- snippets for rails related modes

;; Copyright (C) 2006 Dmitry Galinsky <dima dot exe at gmail dot com>

;; Authors: Dmitry Galinsky <dima dot exe at gmail dot com>,

;; Keywords: ruby rails languages oop
;; $URL: svn+ssh://rubyforge/var/svn/emacs-rails/trunk/rails-snippets.el $
;; $Id: rails-snippets.el 155 2007-04-01 17:37:48Z dimaexe $

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

(require 'snippet)

(defconst rails-snippets-feature:list
  '((0 "ruby")
    (1 "loops" ruby-mode-abbrev-table
       ("while" "while $${condition}\n$>$.\nend$>" "while ... end")
       ("when" "when $${condition}\n$>$." "when ...")
       ("w" "attr_writer :$${attr_names}" "attr_writer ...")
       ("upt" "upto($${0}) { |$${n}|$. }" "upto(1.0/0.0) { |n| ... }")
       ("until" "until $${condition}\n$>$.\nend$>" "until ... end")
       ("tim" "times { |$${n}|$. }" "times { |n| ... }")
       ("ste" "step($${2}) { |$${n}|$. }" "step(2) { |e| ... }")
       ("forin" "for $${element} in $${collection}\n$>$${element}.$.\nend$>" "for ... in ... end")
       ("dow" "downto($${0}) { |$${n}|$. }" "downto(0) { |n| ... }")) ; loops
    (1 "general" ruby-mode-abbrev-table
       ("ha" "{ $>:$. }" "{ :key => 'value' }")
       (":"  ":$${key} => '$${value}'" ":key => 'value'")
       ("yl" "File.open($${yaml}) { |$${file}| YAML.load($${file}) }" "YAML.load(file)")
       ("yd" "File.open($${yaml}, \"w\") { |$${file}| YAML.dump($${obj}, $${file}) }" "YAML.dump(..., file)")
       ("y" " :yields: $${arguments}" ":yields:")
       ("verren" "verify :only => [:$${1}], :method => :post, :render => {:status => 500, :text => \"use HTTP-POST\"}\n" "verify -- render")
       ("verred" "verify :only => [:$${1}], :session => :user, :params => :id, :redirect_to => {:action => '$${index}'}\n" "verify -- redirect")
       ("tra" "transaction$${1} { $. }" "transaction( ... ) { ... }")
       ("sub" "sub(/$${pattern}/) { |$${match}|$. }" "sub(/.../) { |match| ... }")
       ("sca" "scan(/$${pattern}/) { |$${match}| $. }" "scan(/.../) { |match| ... }")
       ("rep" "results.report(\"$${name}:\") { TESTS.times { $. } }" "results.report(...) { ... }")
       ("rb" "#!/usr/bin/env ruby -w\n\n" "#!/usr/local/bin/ruby -w")
       ("r" "attr_reader :$${attr_names}" "attr_reader ...")
       ("pn" "PStore.new($${file_name})" "PStore.new( ... )")
       ("patfh" "File.join(File.dirname(__FILE__), *%w[$${here}])" "path_from_here( ... )")
       ("ope" "open($${pipe}) { |$${io}| $. }" "open(\"path/or/url\", \"w\") { |io| ... }")
       ("ml" "File.open($${dump}) { |$${file}| Marshal.load($${file}) }" "Marshal.load(obj)")
       ("min" "min { |a, b| $. }" "min { |a, b| ... }")
       ("max" "max { |a, b| $. }" "max { |a, b| ... }")
       ("md" "File.open($${dump}, \"w\") { |$${file}| Marshal.dump($${obj}, $${file}) }" "Marshal.dump(..., file)")
       ("lam" "lambda { |$${args}|$. }" "lambda { |args| ... }")
       ("gsu" "gsub(/$${pattern}/) { |$${match}|$. }" "gsub(/.../) { |match| ... }")
       ("gre" "grep($${pattern}) { |$${match}| $. }" "grep(/pattern/) { |match| ... }")
       ("fl" "flunk('$${message}')" "flunk(...)")
       ("file" "File.foreach($${file}) { |$${line}| $. }" "File.foreach (\"...\") { |line| ... }")
       ("dir" "Dir.glob($${glob}) { |$${file}| $. }" "Dir.glob(\"...\") { |file| ... }")
       ("b" "=begin rdoc\n$>$.\n=end" "New Block")
       ("begin" "begin\n$>$${paste}\nrescue $${Exception} => $${e}\n$>$.\nend$>\n" "begin ... rescue ... end")
       ("bm" "TESTS = $${10_000}\nBenchmark.bmbm($${10}) do |results|\n  $.\nend$>" "Benchmark.bmbm(...) do ... end")
       ("am" "alias_method :$${new_name}, :$${old_name}" "alias_method ...")
       ("amc" "alias_method_chain :$${first_method}, :$${second_method}" "alias_method_chain ...")) ; general
    (1 "definitions" ruby-mode-abbrev-table
       ("ts" "require \"test/unit\"\n\nrequire \"tc_$${test_case_file}\"\nrequire \"tc_$${test_case_file}\"\n" "require \"tc_...\" ...")
       ("tc" "require \"test/unit\"\n\nrequire \"$${library_file_name}\"\n\nclass Test$${amp} < Test::Unit::TestCase\n$>def test_$${case_name}\n$>$>$.\nend$>\nend$>" "class ... < Test::Unit::TestCase ... end")
       ("sin" "class << self; self end" "singleton_class()")
       ("rw" "attr_accessor :$${attr_names}" "attr_accessor ...")
       ("req" "require \"$.\"" "require \"...\"")
       ("modf" "module $${ModuleName}\n$>module ClassMethods\n$>$>$.\nend$>\n$>\n$>extend ClassMethods\n$>\n$>def self.included(receiver)\n$>$>receiver.extend(ClassMethods)\nend$>\n$>\n$>\nend$>" "module ... ClassMethods ... end")
       ("mods" "module $${ModuleName}\n$>$.\nend$>" "module ... end")
       ("modu" "module $${ModuleName}\n$>module_function\n$>\n$>$.\nend$>" "module ... module_function ... end")
       ("mm" "def method_missing(meth, *args, &block)\n$>$.\nend$>" "def method_missing ... end")
       ("hash" "Hash.new { |$${hash}, $${key}| $${hash}[$${key}] = $. }" "Hash.new { |hash, key| hash[key] = ... }")
       ("forw" "extend Forwardable" "extend Forwardable")
       ("enum" "include Enumerable\n\ndef each(&block)\n$>$.\nend$>" "include Enumerable ...")
       ("elsif" "elsif $${condition}\n$>$." "elsif ...")
       ("doo" "do |$${object}|\n$>$.\nend$>" "Insert do |object| ... end")
       ("do" "do\n$>$.\nend$>" "do ... end")
       ("defd" "def_delegator :$${del_obj}, :$${del_meth}, :$${new_name}" "def_delegator ...")
       ("defds" "def_delegators :$${del_obj}, :$${del_methods}" "def_delegators ...")
       ("defs" "def self.$${class_method_name}\n$>$.\nend$>" "def self ... end")
       ("deft" "def test_$${case_name}\n$>$.\nend$>" "def test_ ... end")
       ("dee" "Marshal.load(Marshal.dump($${obj_to_copy}))" "deep_copy(...)")
       ("comp" "include Comparable\n\ndef <=>(other)\n$>$.\nend$>" "include Comparable ...")
       ("cladl" "class $${ClassName} < DelegateClass($${ParentClass})\n$>def initialize$${1}\n$>$>super($${del_obj})\n$>$>\n$>$>$.\nend$>\n$>\n$>\nend$>" "class ... < DelegateClass ... initialize ... end")
       ("clapr" "class $${ClassName} < $${ParentClass}\n$>def initialize$${1}\n$>$>$.\nend$>\n$>\n$>\nend$>" "class ... < ParentClass ... initialize ... end")
       ("clast" "class $${ClassName} < Struct.new(:$${attr_names})\n$>def initialize(*args)\n$>$>super\n$>$>\n$>$>$.\nend$>\n$>\n$>\nend$>" "class ... < Struct ... initialize ... end")
       ("class" "class $${ClassName}\n$>$.\nend$>" "class ... end")
       ("classi" "class $${ClassName}\n$>def initialize$${1}\n$>$>$.\nend$>\n$>\n$>\nend$>" "class ... initialize ... end")
       ("clasf" "class << $${self}\n$>$.\nend$>" "class << self ... end")) ; definitions
    (1 "collections" ruby-mode-abbrev-table
       ("zip" "zip($${enums}) { |$${row}| $. }" "zip(enums) { |row| ... }")
       ("sorb" "sort_by { |$${e}| $. }" "sort_by { |e| ... }")
       ("sor" "sort { |a, b| $. }" "sort { |a, b| ... }")
       ("select" "select { |$${element}| $${element}.$${2} }$." "select element")
       ("sel" "select { |$${e}| $. }" "select { |e| ... }")
       ("reve" "reverse_each { |$${e}| $. }" "reverse_each { |e| ... }")
       ("reject" "reject { |$${element}| $${element}.$. }" "reject element")
       ("rej" "reject { |$${e}| $. }" "reject { |e| ... }")
       ("ran" "sort_by { rand }" "randomize()")
       ("mapwi" "enum_with_index.map { |$${e}, $${i}| $. }" "map_with_index { |e, i| ... }")
       ("map" "map { |$${e}| $. }" "map { |e| ... }")
       ("inject" "inject($${object}) { |$${injection}, $${element}| $${4} }$." "inject object")
       ("inj" "inject($${init}) { |$${mem}, $${var}| $. }" "inject(init) { |mem, var| ... }")
       ("flao" "inject(Array.new) { |$${arr}, $${a}| $${arr}.push(*$${a}) }" "flatten_once()")
       ("fina" "find_all { |$${e}| $. }" "find_all { |e| ... }")
       ("fin" "find { |$${e}| $. }" "find { |e| ... }")
       ("fil" "fill($${range}) { |$${i}|$. }" "fill(range) { |i| ... }")
       ("fet" "fetch($${name}) { |$${key}|$. }" "fetch(name) { |key| ... }")
       ("eawi" "each_with_index { |$${e}, $${i}| $. }" "each_with_index { |e, i| ... }")
       ("eai" "each_index { |$${i}| $. }" "each_index { |i| ... }")
       ("eak" "each_key { |$${key}| $. }" "each_key { |key| ... }")
       ("eal" "each_line$${1} { |$${line}| $. }" "each_line { |line| ... }")
       ("eap" "each_pair { |$${name}, $${val}| $. }" "each_pair { |name, val| ... }")
       ("eas" "each_slice($${2}) { |$${group}| $. }" "each_slice(...) { |group| ... }")
       ("eav" "each_value { |$${val}| $. }" "each_value { |val| ... }")
       ("each" "each { |$${element}| $${element}.$. }" "each element")
       ("eac" "each_cons($${2}) { |$${group}| $. }" "each_cons(...) { |group| ... }")
       ("eab" "each_byte { |$${byte}| $. }" "each_byte { |byte| ... }")
       ("ea" "each { |$${e}| $. }" "each { |e| ... }")
       ("det" "detect { |$${e}| $. }" "detect { |e| ... }")
       ("deli" "delete_if { |$${e}| $. }" "delete_if { |e| ... }")
       ("collect" "collect { |$${element}| $${element}.$. }" "collect element")
       ("col" "collect { |$${e}| $. }" "collect { |e| ... }")
       ("cl" "classify { |$${e}| $. }" "classify { |e| ... }")
       ("array" "Array.new($${10}) { |$${i}|$. }" "Array.new(10) { |i| ... }")
       ("any" "any? { |$${e}| $. }" "any? { |e| ... }")
       ("all" "all? { |$${e}| $. }" "all? { |e| ... }")) ; collections
    (0 "erb" html-mode-abbrev-table html-helper-mode-abbrev-table nxml-mode-abbrev-table
       ("title" "<title>$${title}</title>" "title")
       ("textarea" "<textarea name=\"$${Name}\" rows=\"$${8}\" cols=\"$${40}\">$.</textarea>" "textarea")
       ("table" "<table border=\"$${0}\" $${cellpadding}>\n$><tr><th>$${Header}</th></tr>\n$><tr><td>$${Data}</td></tr>\n</table>" "table")
       ("style" "<style type=\"text/css\" media=\"screen\">\n$>$.\n</style>" "style")
       ("scriptsrc" "<script src=\"$${1}\" type=\"text/javascript\" charset=\"$${utf}\"></script>" "script with source")
       ("script" "<script type=\"text/javascript\" charset=\"utf-8\">\n$>$.\n</script>" "script")
       ("movie" "<object width=\"$${2}\" height=\"$${3}\" classid=\"clsid:02BF25D5-8C17-4B23-BC80-D3488ABDDC6B\" codebase=\"http://www.apple.com/qtactivex/qtplugin.cab\">\n$><param name=\"src\" value=\"$${1}\"/>\n$><param name=\"controller\" value=\"$${4}\"/>\n$><param name=\"autoplay\" value=\"$${5}\"/>\n$><embed src=\"$${movie}\"\n$>$>width=\"$${320}\" height=\"$${240}\"\n$>$>controller=\"$${true}\" autoplay=\"$${true}\"\n$>$>scale=\"tofit\" cache=\"true\"\n$>$>pluginspage=\"http://www.apple.com/quicktime/download/\"\n$>/>\n</object>" "quicktime")
       ("meta" "<meta name=\"$${name}\" content=\"$${content}\"/>" "meta")
       ("mailto" "<a href=\"mailto:$${example}?subject=$${feedback}\">$${email}</a>" "mailto")
       ("link" "<link rel=\"$${stylesheet}\" href=\"$${master}\" type=\"text/css\" media=\"$${screen}\" title=\"$${title}\" charset=\"$${utf}\"/>" "link")
       ("licai" "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${edit}\", :id => $${item} %>" "link_to (controller, action, id)")
       ("lica" "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${index}\" %>" "link_to (controller, action)")
       ("lica" "<%= link_to \"$${text}\", :controller => \"$${items}\", :action => \"$${index}\" %>" "link_to (controller, action)")
       ("liai" "<%= link_to \"$${text}\", :action => \"$${edit}\", :id => $${item} %>" "link_to (action, id)")
       ("lic" "<%= link_to \"$${text}\", :controller => \"$${items}\" %>" "link_to (controller)")
       ("lia" "<%= link_to \"$${text}\", :action => \"$${index}\" %>" "link_to (action)")
       ("input" "<input type=\"$${button}\" name=\"$${some_name}\" value=\"$${3}\"$${id}>" "input")
       ("head" "<head>\n$><meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\"/>\n$><title>$${title}</title>\n$>$.\n</head>" "head")
       ("h" "<h1 id=\"$${alpha}\">$${paste}</h1>" "heading")
       ("ft" "<%= form_tag :action => \"$${update}\" %>\n$.\n<%= end_form_tag %>" "form_tag")
       ("ff" "<%= form_for :$${item}, :action => \"$${update}\" %>\n$.\n<% end %>" "form_for")
       ("form" "<form action=\"$${action}\" method=\"$${post}\" accept-charset=\"utf-8\">\n$>$.\n\n$><p><input type=\"submit\" value=\"Continue &rarr;\"/></p>\n</form>" "form")
       ("dtht" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"\n$>\"http://www.w3.org/TR/html4/strict.dtd\">\n" "HTML -- 4.01 Strict")
       ("dchttr" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"\n$>\"http://www.w3.org/TR/html4/loose.dtd\">\n" "HTML -- 4.01 Transitional")
       ("dcxmlf" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\"\n$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">\n" "XHTML -- 1.0 Frameset")
       ("dcxmls" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n" "XHTML -- 1.0 Strict")
       ("dcxmlt" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\n$>\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n" "XHTML -- 1.0 Transitional")
       ("dcxml1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\"\n$>\"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">\n" "XHTML -- 1.1")
       ("body" "<body id=\"$${id}\" $${onload}>\n$>$.\n</body>" "body")
       ("div" "<div>\n$>$${paste}\n</div>" "div")
       ("%h" "<%=h $${@item} %>" "<% h ... %>")
       ("%if" "<% if $${cond} -%>\n$.\n<% end -%>" "<% if/end %>")
       ("%ifel" "<% if $${cond} -%>\n$.\n<% else -%>\n<% end -%>" "<% if/else/end %>")
       ("%unless" "<% unless $${cond} -%>\n$.\n<% end -%>" "<% unless/end %>")
       ("%for" "<% for $${elem} in @$${list} %>\n$>$.\n<% end %>$>" "<% for/end %>")
       ("%" "<% $. -%>" "<% ... %>")
       ("%%" "<%= $. %>" "<%= ... %>")) ; erb
    (0 "controller" rails-controller-minor-mode-abbrev-table
       ("ru"  "render :update do |page|\n$>$.\nend$>" "render :update ...")
       ("bf"  "before_filter :$${filter}" "refore_filter")
       ("af"  "after_filter :$${filter}" "after_filter")
       ("arf"  "around_filter :$${filter}" "around_filter")) ; controller
    (0 "RESTful" rails-controller-minor-mode-abbrev-table
       rails-view-minor-mode-abbrev-table
       rails-helper-minor-mode-abbrev-table
       rails-functional-test-minor-mode-abbrev-table
       ("rest" "respond_to do |format|\n$>format.html$>$.\nend$>" "respond_to ..." rails-controller-minor-mode-abbrev-table)
       ("rindex" "$${,rails-snippets-feature:rest-index}" "models_url")
       ("rshow" "$${,rails-snippets-feature:rest-show}" "model_url(@model)")
       ("rnew" "$${,rails-snippets-feature:rest-new}" "new_model_url")
       ("redit" "$${,rails-snippets-feature:rest-edit}" "edit_model_url(@model)")
       ("rcreate" "$${,rails-snippets-feature:rest-create}" "models_url")
       ("rupdate" "$${,rails-snippets-feature:rest-update}" "model_url(@model)")
       ("rdestroy" "$${,rails-snippets-feature:rest-destroy}" "model_url(@model)")) ; RESTFul
    (0 "render" rails-controller-minor-mode-abbrev-table
       rails-view-minor-mode-abbrev-table
       rails-helper-minor-mode-abbrev-table
       ("rps" "render :partial => '$${item}', :status => $${500}" "render (partial, status)")
       ("rt" "render :text => '$${render}'" "render (text)")
       ("rtl" "render :text => '$${render}', :layout => '$${layoutname}'" "render (text, layout)")
       ("rtlt" "render :text => '$${render}', :layout => $${true}" "render (text, layout => true)")
       ("rts" "render :text => '$${render}', :status => $${401}" "render (text, status)")
       ("rf" "render :file => '$${filepath}'" "render (file)")
       ("rfu" "render :file => '$${filepath}', :use_full_path => $${false}" "render (file, use_full_path)")
       ("ri" "render :inline => '$${hello}'" "render (inline)")
       ("ril" "render :inline => '$${hello}', :locals => { $${name} => '$${value}'$${4} }" "render (inline, locals)")
       ("rit" "render :inline => '$${hello}', :type => $${rxml}" "render (inline, type)")
       ("rl" "render :layout => '$${layoutname}'" "render (layout)")
       ("rn" "render :nothing => $${true}" "render (nothing)")
       ("rns" "render :nothing => $${true}, :status => $${401}" "render (nothing, status)")
       ("rp" "render :partial => '$${item}'" "render (partial)")
       ("rpc" "render :partial => '$${item}', :collection => $${items}" "render (partial, collection)")
       ("rpl" "render :partial => '$${item}', :locals => { :$${name} => '$${value}'$${4} }" "render (partial, locals)")
       ("rpo" "render :partial => '$${item}', :object => $${object}" "render (partial, object)")
       ("rcea" "render_component :action => '$${index}'" "render_component (action)")
       ("rcec" "render_component :controller => '$${items}'" "render_component (controller)")
       ("rceca" "render_component :controller => '$${items}', :action => '$${index}'" "render_component (controller, action)")
       ("ra" "render :action => '$${index}'" "render (action)")
       ("ral" "render :action => '$${index}', :layout => '{default}'" "render (action, layout)")) ; render
    (0 "redirect_to" rails-controller-minor-mode-abbrev-table
       rails-view-minor-mode-abbrev-table
       rails-helper-minor-mode-abbrev-table
       ("rea" "redirect_to :action => '$${index}'" "redirect_to (action)")
       ("reai" "redirect_to :action => '$${show}', :id => $${item}" "redirect_to (action, id)")
       ("rec" "redirect_to :controller => '$${items}'" "redirect_to (controller)")
       ("reca" "redirect_to :controller => '$${items}', :action => '$${list}'" "redirect_to (controller, action)")
       ("recai" "redirect_to :controller => '$${items}', :action => '$${show}', :id => $${item}" "redirect_to (controller, action, id)")) ; redirecto_to
    (0 "rails" ruby-mode-abbrev-table
       ("rdl" "RAILS_DEFAULT_LOGGER.debug '$${message}'$." "RAILS_DEFAULT_LOGGER.debug")
       ("nr" "@$${item}.new_record?" "item.new_record?")) ; rails
    (0 "model" rails-model-minor-mode-abbrev-table
       ("va" "validates_associated :$${attribute}" "validates_associated")
       ("vc" "validates_confirmation_of :$${attribute}" "validates_confirmation_of")
       ("ve" "validates_exclusion_of :$${attribute}" "validates_exclusion_of")
       ("vu" "validates_uniqueness_of :$${attribute}" "validates_uniqueness_of")
       ("vpif" "validates_presence_of :$${attribute}, :if => proc { |obj| $${condition} }" "validates_presence_of if")
       ("vp" "validates_presence_of :$${attribute}" "validates_presence_of")
       ("vl" "validates_length_of :$${attribute}, :within => $${20}" "validates_length_of")
       ("bt" "belongs_to :$${model}" "belongs_to")
       ("hm" "has_many :$${objects}" "has_many")
       ("hmt" "has_many :$${objects}, :through => :$${,rails-snippets-feature:prev-has-many-table-name}" "has_many :through")
       ("ho" "has_one :$${object}" "has_one")
       ("habtm" "has_and_belongs_to_many :$${object}" "has_and_belongs_to_many")) ; model
    (0 "migrations" rails-migration-minor-mode-abbrev-table
       ("tcls" "t.column :$${title}, :$${string}\n$>tcls$." "create several columns")
       ("tcl" "t.column :$${title}, :$${string}$." "create column")
       ("tcln" "t.column :$${title}, :$${string}, :null => false$." "create column :null => false")
       ("acl" "add_column :$${,rails-snippets-feature:migration-table-name}, :$${column}, :$${string}" "add column")
       ("ai" "add_index :$${,rails-snippets-feature:migration-table-name}, $${column}" "add index")
       ("aiu" "add_index :$${,rails-snippets-feature:migration-table-name}, $${column}, :unique => true" "add unique index")
       ("rmcl" "remove_column :$${,rails-snippets-feature:migration-table-name}, :$${column}" "remove column")
       ("recl" "rename_column :$${column}, :$${new_column}" "rename column")
       ("dt" "drop_table :$${,rails-snippets-feature:migration-table-name}$." "drop table")
       ("ct" "create_table :$${,rails-snippets-feature:migration-table-name} do |t|\n$>tcls$.\nend$>" "create_table")
       ("ret" "rename_table :$${,rails-snippets-feature:migration-table-name}, :$${new_name}$." "rename table")) ; migrations
    (0 "environment" ruby-mode-abbrev-table
       ("logd" "logger.debug '$${message}'$." "logger.debug")
       ("loge" "logger.error '$${message}'$." "logger.error")
       ("logf" "logger.fatal '$${message}'$." "logger.fatal")
       ("logi" "logger.info '$${message}'$." "logger.info")
       ("logw" "logger.warn '$${message}'$." "logger.warn")
       ("par" "params[:$${id}]" "params[...]")
       ("session" "session[:$${User}]" "session[...]")
       ("flash" "flash[:$${notice}] = '$${Successfully}'$." "flash[...]")) ; environment
    (0 "tests" rails-functional-test-minor-mode-abbrev-table rails-unit-test-minor-mode-abbrev-table
       ("fix" "$${,rails-snippets-feature:fixture}(:$${one})$." "models(:name)")) ; functional tests
    (0 "assertions" rails-functional-test-minor-mode-abbrev-table rails-unit-test-minor-mode-abbrev-table
       ("art" "assert_redirected_to :action => '$${index}'" "assert_redirected_to")
       ("as" "assert $${test}" "assert(...)")
       ("asa" "assert assigns(:$${,rails-snippets-feature:model-name})" "assert assigns(...)")
       ("ase" "assert_equal $${expected}, $${actual}" "assert_equal(...)")
       ("asid" "assert_in_delta $${expected_float}, $${actual_float}, $${20}" "assert_in_delta(...)")
       ("asio" "assert_instance_of $${ExpectedClass}, $${actual_instance}" "assert_instance_of(...)")
       ("asko" "assert_kind_of $${ExpectedKind}, $${actual_instance}" "assert_kind_of(...)")
       ("asm" "assert_match(/$${expected_pattern}/, $${actual_string})" "assert_match(...)")
       ("asn" "assert_nil $${instance}" "assert_nil(...)")
       ("asne" "assert_not_equal $${unexpected}, $${actual}" "assert_not_equal(...)")
       ("asnm" "assert_no_match(/$${unexpected_pattern}/, $${actual_string})" "assert_no_match(...)")
       ("asnn" "assert_not_nil $${instance}" "assert_not_nil(...)")
       ("asnr" "assert_nothing_raised $${Exception}  { $. }" "assert_nothing_raised(...) { ... }")
       ("asns" "assert_not_same $${unexpected}, $${actual}" "assert_not_same(...)")
       ("asnt" "assert_nothing_thrown { $. }" "assert_nothing_thrown { ... }")
       ("aso" "assert_operator $${left}, :$${operator}, $${right}" "assert_operator(...)")
       ("asr" "assert_raise $${Exception} { $. }" "assert_raise(...) { ... }")
       ("asre" "assert_response :$${success}" "assert_response")
       ("asrt" "assert_respond_to $${object}, :$${method}" "assert_respond_to(...)")
       ("ass" "assert_same $${expected}, $${actual}" "assert_same(...)")
       ("assd" "assert_send [$${object}, :$${message}, $${args}]" "assert_send(...)")
       ("ast" "assert_throws :$${expected} { $. }" "assert_throws(...) { ... }")
       ("astm" "assert_template '$${index}'" "assert_template"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Snippets functions
;;

(defmacro rails-snippets-feature:create-lambda (str)
  `(lambda () (interactive) (snippet-insert ,(symbol-value str))))

(defun rails-snippets-feature:create-keymap ()
  (let ((keymap (make-sparse-keymap "Snippets"))
        ret level stack)
    (dolist (line rails-snippets-feature:list)
      (let ((cur-level (nth 0 line))  ; current the menu livel
            (menu-item (nth 1 line))  ; current the menu item name
            (line (cddr line))        ; skip level and menu name
            (abbrev-tables))
        ;; fill stack
        (cond
         ((not level)
          (setq level cur-level)
          (setq stack (list menu-item)))
         ((= cur-level level)
          (setq stack (append (reverse (cdr (reverse stack))) (list menu-item))))
         ((> cur-level level)
          (setq level cur-level)
          (setq stack (append stack (list menu-item))))
         ((< cur-level level)
          (setq stack (append (reverse (nthcdr (+ 1 (- level cur-level)) (reverse stack)))
                              (list menu-item)))))
        (let ((cur-keymap (vconcat (mapcar #'make-symbol stack))))
          ;; make a menu entry for group of snippets
          (define-key keymap cur-keymap
            (cons menu-item (make-sparse-keymap menu-item)))
          ;; scan abbrev tables
          (while (not (listp (car line)))
            (setq abbrev-tables (append abbrev-tables (list (car line))))
            (setq line (cdr line)))
          (when abbrev-tables
            ;; sort and scan snippets
            (dolist (snip-line (sort line (lambda(x y) (not (string< (car x)(car y))))))
              (let* ((abbr (nth 0 snip-line))
                     (snip (nth 1 snip-line))
                     (desc (nth 2 snip-line))
                     (loc-abbrev-table (nth 3 snip-line))
                     (abbrev-tables (if loc-abbrev-table
                                        (list loc-abbrev-table)
                                      abbrev-tables))
                     (compiled-snip (rails-snippets-feature:create-lambda snip)))
                ;; create a menu entry for a snippet
                (define-key keymap (vconcat cur-keymap (list (make-symbol abbr)))
                  (cons (format "%s \t%s" abbr desc) compiled-snip))
                ;; create abbrevs for a snippet
                (dolist (table abbrev-tables)
                  (unless (boundp table)
                    (define-abbrev-table table ()))
                  (define-abbrev (symbol-value table) abbr "" compiled-snip))))))))
    keymap))

(defadvice snippet-insert (before snippet-insert-before-advice first (template) activate)
  (let ((pos 0))
    (while (setq pos (string-match (snippet-field-regexp) template pos))
      (let ((match (match-string 2 template))
            (beg (match-beginning 2))
            (end (match-end 2))
            (repl))
        (setq pos end)
        (when (= 44 (car (string-to-list match))) ;; 44 - [,]
          (save-match-data
            (setq repl (apply (intern (substring match 1)) (list)))))
        (when repl
          (setq template
                (concat (substring template 0 beg)
                        repl
                        (substring template end (length template))))
          (setq pos (- pos
                       (- (length match) (length repl)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Functions for dynamic snippets
;;

(defun rails-snippets-feature:migration-table-name ()
  (let (str)
    (string=~ "[0-9]+_create_\\([^\\.]+\\)\\.rb$" (buffer-name)
              (setq str $1))
    (if str str "table")))

(defun rails-snippets-feature:prev-has-many-table-name ()
  (save-excursion
    (if (search-backward-regexp "has_many :\\(\\w+\\)" nil t)
        (match-string-no-properties 1)
      "table")))

(defun rails-snippets-feature:fixture ()
  (let ((controller (rails-core:current-controller))
        (model (rails-core:current-model)))
    (cond
     (controller (downcase controller))
     (model (pluralize-string (downcase model)))
     (t "fixture"))))

(defun rails-snippets-feature:model-name ()
  (let ((controller (rails-core:current-controller)))
    (if controller
        (singularize-string (downcase controller))
      "model")))

(defun rails-snippets-feature:rest (action)
  (when-bind
   (controller (rails-core:current-controller))
   (let* ((plural (downcase (pluralize-string controller)))
          (singular (downcase (singularize-string controller)))
          (model (concat "@" singular)))
     (case action
       (:index
        (tooltip-show (format "GET /%s" plural))
        (format "%s_url" plural))
       (:show
        (tooltip-show (format "GET /%s/1" plural))
        (format "%s_url(%s)" singular model))
       (:new
        (tooltip-show (format "GET /%s/new" plural))
        (format "new_%s_url" singular))
       (:edit
        (tooltip-show (format "GET /%s/1;edit" plural))
        (format "edit_%s_url(%s)" singular model))
       (:create
        (tooltip-show (format "POST /%s" plural))
        (format "%s_url" plural))
       (:update
        (tooltip-show (format "PUT /%s/1" plural))
        (format "%s_url(%s)" singular model))
       (:destroy
        (tooltip-show (format "DELETE /%s/1" plural))
        (format "%s_url(%s)" singular model))))))

(defun rails-snippets-feature:rest-index ()
  (rails-snippets-feature:rest :index))

(defun rails-snippets-feature:rest-show ()
  (rails-snippets-feature:rest :show))

(defun rails-snippets-feature:rest-new ()
  (rails-snippets-feature:rest :new))

(defun rails-snippets-feature:rest-edit ()
  (rails-snippets-feature:rest :edit))

(defun rails-snippets-feature:rest-create ()
  (rails-snippets-feature:rest :create))

(defun rails-snippets-feature:rest-update ()
  (rails-snippets-feature:rest :update))

(defun rails-snippets-feature:rest-destroy ()
  (rails-snippets-feature:rest :destroy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Install function
;;

(defun rails-snippets-feature:install ()
  (define-key rails-minor-mode-map
    [menu-bar rails-snippets]
    (cons "Snippets" (rails-snippets-feature:create-keymap))))

(provide 'rails-snippets-feature)
