## ri-emacs.rb helper script for use with ri-ruby.el
#
# Author: Kristof Bastiaensen <kristof@vleeuwen.org>
#
#
#    Copyright (C) 2004,2006 Kristof Bastiaensen
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#----------------------------------------------------------------------
#
#  For information on how to use and install see ri-ruby.el
#

require 'rdoc/ri/ri_paths'
require 'rdoc/ri/ri_cache'
require 'rdoc/ri/ri_util'
require 'rdoc/ri/ri_reader'
require 'rdoc/ri/ri_formatter'
require 'rdoc/ri/ri_display'

class DefaultDisplay
  def full_params(method)
    method.params.split(/\n/).each do |p|
      p.sub!(/^#{method.name}\(/o,'(')
      unless p =~ /\b\.\b/
        p = method.full_name + p
      end
      @formatter.wrap(p) 
      @formatter.break_to_newline
    end
  end
end

class RiEmacs
   Options = Struct.new(:formatter, :use_stdout, :width)

   def initialize(paths)
      options = Options.new

      options.use_stdout = true
      options.formatter = RI::TextFormatter.for("ansi")
      options.width = 72

      begin
         require 'rubygems'
         Dir["#{Gem.path}/doc/*/ri"].each do |path|
            RI::Paths::PATH << path
         end
      rescue LoadError
      end 

      paths = paths || RI::Paths::PATH

      @ri_reader = RI::RiReader.new(RI::RiCache.new(paths))
      @display = RiDisplay.new(options)
   end

   def lookup_keyw(keyw)
      begin
         @desc = NameDescriptor.new(keyw)
      rescue RiError => e
         return false
      end
      @namespaces = @ri_reader.top_level_namespace

      container = @namespaces
      for class_name in @desc.class_names
         return false if container.empty?
         @namespaces = @ri_reader.lookup_namespace_in(class_name, container)
         container = @namespaces.find_all {|m| m.name == class_name}
      end

      if @desc.method_name.nil?
         if [?., ?:, ?#].include? keyw[-1]
            @namespaces = container
            is_class_method = case keyw[-1]
                              when ?.: nil
                              when ?:: true
                              when ?#: false
                              end
            @methods = @ri_reader.find_methods("", is_class_method,
                                               container)
            return false if @methods.empty?
         else
            @namespaces = @namespaces.find_all{ |n| n.name.index(class_name).zero? }
            return false if @namespaces.empty?
            @methods = nil
         end
      else
         return false if container.empty?
         @namespaces = container
         @methods = @ri_reader.find_methods(@desc.method_name,
                                            @desc.is_class_method,
                                            container)
         @methods = @methods.find_all { |m|
            m.name.index(@desc.method_name).zero? }
         return false if @methods.empty?
      end
      
      return true
   end

   def completion_list(keyw)
      return @ri_reader.full_class_names if keyw == ""
      
      return nil unless lookup_keyw(keyw)

      if @methods.nil?
         return @namespaces.map{ |n| n.full_name }
      elsif @desc.class_names.empty?
         return @methods.map { |m| m.name }.uniq
      else
         return @methods.map { |m| m.full_name }
      end
   end

   def complete(keyw, type)
      list = completion_list(keyw)

      if list.nil?
         return "nil"
      elsif type == :all
         return "(" + list.map { |w| w.inspect }.join(" ") + ")"
      elsif type == :lambda
         if list.find { |n|
               n.split(/(::)|#|\./) == keyw.split(/(::)|#|\./) }
            return "t"
         else
            return "nil"
         end
      # type == try
      elsif list.size == 1 and
            list[0].split(/(::)|#|\./) == keyw.split(/(::)|#|\./)
         return "t"
      end

      first = list.shift;
      if first =~ /(.*)((?:::)|(?:#))(.*)/
         other = $1 + ($2 == "::" ? "#" : "::") + $3
      end
                        
      len = first.size
      match_both = false
      list.each do |w|
         while w[0, len] != first[0, len]
            if other and w[0, len] == other[0, len]
               match_both = true
               break
            end
            len -= 1
         end
      end

      if match_both
         return other.sub(/(.*)((?:::)|(?:#))/) {
            $1 + "." }[0, len].inspect
      else
         return first[0, len].inspect
      end
   end

   def display_info(keyw)
      return false unless lookup_keyw(keyw)
      
      if @methods.nil?
         @namespaces = @namespaces.find_all { |n| n.full_name == @desc.full_class_name }
         return false if @namespaces.empty?
         klass = @ri_reader.get_class(@namespaces[0])
         @display.display_class_info(klass, @ri_reader)
      else
         @methods = @methods.find_all { |m| m.name == @desc.method_name }
         return false if @methods.empty?
         meth = @ri_reader.get_method(@methods[0])
         @display.display_method_info(meth)
      end

      return true
   end

   def display_args(keyw)
      return nil unless lookup_keyw(keyw)
      return nil unless @desc.class_names.empty?

      @methods = @methods.find_all { |m| m.name == @desc.method_name }
      return false if @methods.empty?
      @methods.each do |m|
        meth = @ri_reader.get_method(m)
        @display.full_params(meth)
      end

      return true
   end

   # return a list of classes for the method keyw
   # return nil if keyw has already a class
   def class_list(keyw, rep='\1')
      return nil unless lookup_keyw(keyw)
      return nil unless @desc.class_names.empty?

      @methods = @methods.find_all { |m| m.name == @desc.method_name }

      return "(" + @methods.map do |m|
         "(" + m.full_name.sub(/(.*)(#|(::)).*/,
                                rep).inspect + ")"
      end.uniq.join(" ") + ")"
   end

   # flag means (#|::) 
   # return a list of classes and flag for the method keyw
   # return nil if keyw has already a class
   def class_list_with_flag(keyw)
     class_list(keyw, '\1\2')
   end
end

class Command
   def initialize(ri)
      @ri = ri
   end

   Command2Method = {
      "TRY_COMPLETION" => :try_completion,
      "COMPLETE_ALL" => :complete_all,
      "LAMBDA" => :lambda,
      "CLASS_LIST" => :class_list,
      "CLASS_LIST_WITH_FLAG" => :class_list_with_flag,
      "DISPLAY_ARGS" => :display_args,
      "DISPLAY_INFO" => :display_info}
      
   def read_next
      line = STDIN.gets
      cmd, param = /(\w+)(.*)$/.match(line)[1..2]
      method = Command2Method[cmd]
      fail "unrecognised command: #{cmd}" if method.nil?
      send(method, param.strip)
   end

   def try_completion(keyw)
      STDOUT.puts @ri.complete(keyw, :try)
   end

   def complete_all(keyw)
      STDOUT.puts @ri.complete(keyw, :all)
   end

   def lambda(keyw)
      STDOUT.puts @ri.complete(keyw, :lambda)
   end

   def class_list(keyw)
      STDOUT.puts @ri.class_list(keyw)
   end

   def class_list_with_flag(keyw)
      STDOUT.puts @ri.class_list_with_flag(keyw)
   end

   def display_args(keyw)
      @ri.display_args(keyw)
      STDOUT.puts "RI_EMACS_END_OF_INFO"
   end

   def display_info(keyw)
      @ri.display_info(keyw)
      STDOUT.puts "RI_EMACS_END_OF_INFO"
   end

   def test
      [:try, :all, :lambda].each do |t|
         @ri.complete("each", t) or
            fail "@ri.complete(\"each\", #{t.inspect}) returned nil"
      end
      @ri.display_info("Array#each") or
         raise 'display_info("Array#each") returned false'
   end
end

arg = ARGV[0]

if arg == "--test"
   cmd = Command.new(RiEmacs.new(nil))
   cmd.test
   puts "Test succeeded"
else
   cmd = Command.new(RiEmacs.new(arg))
   puts 'READY'
   loop { cmd.read_next }
end
