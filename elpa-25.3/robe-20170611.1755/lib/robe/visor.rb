require 'forwardable'

module Robe
  class Visor
    extend Forwardable

    def each_object(*args)
      ObjectSpace.each_object(*args).reject { |m| m.__singleton_class__? }
    end

    def descendants(cls)
      each_object(cls.singleton_class).to_a - [cls]
    end

    def resolve_context(name, mod)
      return resolve_const(mod) unless name
      unless name =~ /\A::/
        nesting = mod ? mod.split("::") : []
        resolve_path_elems(nesting).reverse.each do |elem|
          begin
            return elem.const_get(name)
          rescue NameError
          end
        end
      end
      resolve_const(name)
    end

    def resolve_const(name)
      resolve_path(name).last
    end

    def resolve_path(name)
      return [] unless name
      return [ARGF.class] if name == "ARGF.class"
      if %w(IO::readable IO::writable).include?(name)
        return [StringIO.included_modules.find { |m| m.name == name }]
      end
      nesting = name.split("::")
      nesting.shift if nesting[0].empty?
      resolve_path_elems(nesting)
    end

    def resolve_path_elems(nesting, init = Object)
      c = init; ary = []
      begin
        nesting.each do |name|
          ary << (c = c.const_get(name))
        end
        ary
      rescue NameError
        []
      end
    end
  end
end
