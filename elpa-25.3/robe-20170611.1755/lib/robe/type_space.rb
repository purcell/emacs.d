require 'robe/sash'

module Robe
  class TypeSpace
    attr_reader :visor, :target_type, :instance

    def initialize(visor, target, mod, instance, superc)
      @visor = visor
      @instance = instance
      @superc = superc
      guess_target_type(target, mod)
    end

    def scan_with(scanner)
      return unless obj = target_type
      modules = obj.ancestors
      modules -= obj.included_modules unless instance

      if @superc
        modules -= [obj]
      else
        modules += visor.descendants(obj).to_a
      end

      modules.push(Kernel) if instance && !obj.is_a?(Class)

      if instance
        if defined? ActiveSupport::Concern and obj.is_a?(ActiveSupport::Concern)
          deps = obj.instance_variable_get("@_dependencies")
          modules += deps if deps
        end
      end

      scanner.scan(modules, instance, !instance)

      unless instance
        singleton_ancestors = obj.singleton_class.ancestors

        if RUBY_VERSION >= "2.1.0"
          # Ruby 2.1 includes all singletons in the ancestors chain
          singleton_ancestors.reject!(&:__singleton_class__?)
        end

        scanner.scan(singleton_ancestors, true, false)
      end
    end

    private

    def guess_target_type(target, mod)
      @target_type = visor.resolve_context(target, mod)
      if @target_type && !(@target_type.is_a? Module)
        @target_type, @instance = @target_type.class, true
      end
    end
  end
end
