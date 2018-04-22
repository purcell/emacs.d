require 'robe/core_ext'

module Robe
  class Sash
    class IncludesTracker
      def self.method_owner_and_inst(owner, name_cache)
        includers = maybe_scan

        mod, inst = includers[owner].first

        if mod
          [name_cache[mod], inst]
        else
          [nil, true]
        end
      end

      def self.reset!
        @@hosts = nil
      end

      private

      def self.maybe_scan
        includers = @@hosts

        unless includers
          @@hosts = includers = Hash.new { |h, k| h[k] = [] }

          ObjectSpace.each_object(Module) do |cl|
            next unless cl.respond_to?(:included_modules)
            next if cl.__singleton_class__?
            cl.included_modules.each { |mod| includers[mod] << [cl, true] }
            sc = cl.__singleton_class__
            sc.included_modules.each { |mod| includers[mod] << [cl, nil] }
          end
        end

        includers
      end

      if Module.respond_to?(:prepend)
        module Invalidator
          def included(other)
            IncludesTracker.reset!
            super(other)
          end

          def extended(other)
            IncludesTracker.reset!
            super(other)
          end
        end

        Module.send(:prepend, Invalidator)
      else
        Module.class_eval do
          alias_method :__orig_included, :included
          alias_method :__orig_extended, :extended

          # Cannot hook into this method without :prepend.
          def included(other)
            IncludesTracker.reset!
            __orig_included(other)
          end

          def extended(other)
            IncludesTracker.reset!
            __orig_extended(other)
          end
        end
      end
    end
  end
end
