require 'robe/visor'

module Robe
  class JVisor < Visor
    def each_object(mod)
      # http://jira.codehaus.org/browse/JRUBY-7027
      ObjectSpace.each_object(mod).select { |m| m.respond_to? :name }
    end

    def descendants(cls)
      ObjectSpace.each_object(Class).select { |c| c < cls }
    end
  end
end
