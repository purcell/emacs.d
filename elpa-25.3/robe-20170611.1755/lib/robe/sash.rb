require 'robe/sash/doc_for'
require 'robe/type_space'
require 'robe/scanners'
require 'robe/visor'
require 'robe/jvisor'
require 'robe/core_ext'
require 'robe/sash/includes_tracker'

module Robe
  class Sash
    attr_accessor :visor, :name_cache

    def initialize(visor = pick_visor)
      @visor = visor
      init_name_cache
    end

    def class_locations(name, mod)
      locations = {}
      if (obj = visor.resolve_context(name, mod)) and obj.is_a? Module
        methods = obj.methods(false).map { |m| obj.method(m) } +
                  obj.__instance_methods__(false).map { |m| obj.instance_method(m) }
        methods.each do |m|
          if loc = m.source_location
            path = loc[0]
            locations[path] ||= 0
            locations[path] += 1
          end
        end
      end
      if defined? Class.class_attribute and Class != obj
        locations.delete Class.method(:class_attribute).source_location[0]
      end
      locations.keys.sort { |k1, k2| -(locations[k1] <=> locations[k2]) }
    end

    def modules
      visor.each_object(Module).map { |mod| name_cache[mod] }.compact
    end

    def targets(obj)
      obj = visor.resolve_const(obj)
      if obj.is_a? Module
        module_methods = obj.methods.map { |m| method_spec(obj.method(m)) }
        instance_methods = (obj.__instance_methods__ +
                            obj.__private_instance_methods__(false))
          .map { |m| method_spec(obj.instance_method(m)) }
        [name_cache[obj]] + module_methods + instance_methods
      else
        self.targets(obj.class.to_s)
      end
    end

    def find_method(mod, inst, sym)
      mod.__send__(inst ? :instance_method : :method, sym)
    end

    def find_method_owner(mod, inst, sym)
      begin
        find_method(mod, inst, sym).owner
      rescue NameError
      end
    end

    def method_spec(method)
      owner, inst = method.owner, nil
      if owner.__singleton_class__?
        name = owner.to_s[/Class:([A-Z][^\(> ]*)/, 1] # defined in an eigenclass
      elsif name = name_cache[owner]
        inst = true
      elsif !owner.is_a?(Class)
        name, inst = IncludesTracker.method_owner_and_inst(owner, name_cache)
      end
      # XXX: We can speed this up further by only returning the
      # method's (or owner's) object_id here, and resolve the "real"
      # host's name only when it's really needed. But that would break
      # the current 'meta' impl in company-robe, for one thing.
      [name, inst, method.name, method.parameters] + method.source_location.to_a
    end

    def doc_for(mod, inst, sym)
      resolved = visor.resolve_const(mod)
      DocFor.new(find_method(resolved, inst, sym.to_sym)).format
    end

    def method_targets(name, target, mod, instance, superc, conservative)
      sym = name.to_sym
      space = TypeSpace.new(visor, target, mod, instance, superc)
      special_method = superc

      scanner = ModuleScanner.new(sym, special_method || !target)

      space.scan_with(scanner)
      targets = scanner.candidates

      if targets
        targets.delete(Class.instance_method(:new))
        filter_targets!(space, targets, instance, sym)
      end

      sc = space.target_type.singleton_class

      if !instance && (sym == :new) && targets.all? { |t| t.owner < sc }
        ctor_space   = TypeSpace.new(visor, target, mod, true, superc)
        ctor_scanner = ModuleScanner.new(:initialize, true)
        ctor_space.scan_with(ctor_scanner)
        ctor_targets = ctor_scanner.candidates
        filter_targets!(ctor_space, ctor_targets, true, :initialize)
        targets += ctor_targets
      end

      if targets.empty? && (target || !conservative) && !special_method
        unless target
          scanner.scan_methods(Kernel, :__private_instance_methods__)
        end
        scanner.check_private = false
        scanner.scan(visor.each_object(Module), true, true)
        targets = scanner.candidates
      end

      targets.map { |method| method_spec(method) }
        .sort_by { |(mname)| mname ? mname.scan(/::/).length : 99 }
    end

    def filter_targets!(space, targets, instance, sym)
      owner = find_method_owner(space.target_type, instance, sym)
      if owner
        targets.reject! do |method|
          !(method.owner <= owner) &&
            targets.find { |other| other.owner < method.owner }
        end
      end
    end

    def complete_method(prefix, target, mod, instance)
      space = TypeSpace.new(visor, target, mod, instance, nil)
      scanner = MethodScanner.new(prefix, !target)

      space.scan_with(scanner)

      if scanner.candidates.empty?
        scanner.check_private = false
        scanner.scan(visor.each_object(Module), true, true)
      end

      scanner.candidates.map { |m| method_spec(m) }
    end

    def complete_const(prefix, mod)
      colons = prefix.rindex("::")
      tail = colons ? prefix[colons + 2..-1] : prefix
      if !colons
        path = [Object]
        path += visor.resolve_path(mod) if mod
        path.flat_map do |m|
          complete_const_in_module(tail, m)
        end
      else
        base_name = prefix[0..colons + 1]
        base = unless colons == 0
                 if mod
                   visor.resolve_context(base_name[0..-3], mod)
                 else
                   visor.resolve_const(base_name)
                 end
               end
        complete_const_in_module(tail, base || Object)
      end.map { |c| "#{base_name}#{c}" }
    end

    def complete_const_in_module(tail, base)
      base.constants.grep(/^#{Regexp.escape(tail)}/)
    end

    def rails_refresh
      if defined?(Rails.application.reloader)
        Rails.application.reloader.reload!
      else
        ActionDispatch::Reloader.cleanup!
        ActionDispatch::Reloader.prepare!
      end
      Rails.application.eager_load!
      init_name_cache
    end

    def load_path
      $LOAD_PATH
    end

    def ping
      "pong"
    end

    def call(path, body)
      _, endpoint, *args = path.split("/").map { |s| s == "-" ? nil : s }
      value = public_send(endpoint.to_sym, *args)
      value.to_json
    end

    private

    def pick_visor
      if RUBY_ENGINE == "jruby"
        JVisor.new
      else
        Visor.new
      end
    end

    def init_name_cache
      # https://www.ruby-forum.com/topic/167055
      @name_cache = Hash.new { |h, mod| h[mod] = mod.__name__ }
    end
  end
end
