class Module
  unless method_defined?(:__name__)
    alias_method :__name__, :name
  end

  if method_defined?(:singleton_class?)
    alias_method :__singleton_class__?, :singleton_class?
  else
    def __singleton_class__?
      self != Class && ancestors.first != self
    end
  end

  unless method_defined?(:__singleton_class__)
    alias_method :__singleton_class__, :singleton_class
  end

  unless method_defined?(:__include__?)
    alias_method :__include__?, :include?
  end

  unless method_defined?(:__instance_methods__)
    alias_method :__instance_methods__, :instance_methods
  end

  unless method_defined?(:__public_instance_methods__)
    alias_method :__public_instance_methods__, :public_instance_methods
  end

  unless method_defined?(:__protected_instance_methods__)
    alias_method :__protected_instance_methods__, :protected_instance_methods
  end

  unless method_defined?(:__private_instance_methods__)
    alias_method :__private_instance_methods__, :private_instance_methods
  end
end
