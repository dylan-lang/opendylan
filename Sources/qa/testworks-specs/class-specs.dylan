Module:       testworks-specs
Synopsis:     A library for building specification test suites
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Class specs

define class <class-spec> (<definition-spec>)
  constant slot class-spec-class :: <class>,
    required-init-keyword: class:;
  constant slot class-spec-superclasses :: <sequence>,
    required-init-keyword: superclasses:;
  slot class-spec-modifiers :: <sequence> = #[],
    init-keyword: modifiers:;
end class <class-spec>;

define method initialize (this :: <class-spec>, #key)
  next-method();
  let modifiers = this.class-spec-modifiers;
  // Ensure no conflicting modifiers were specified.
  if ((member?(#"sealed", modifiers) & member?(#"open", modifiers))
      | (member?(#"primary", modifiers) & member?(#"free", modifiers))
      | (member?(#"abstract", modifiers) & member?(#"concrete", modifiers)))
    error("Conflicting modifiers specified for class %s",
	  this.class-spec-class);
  end if;
  // Classes are concrete by default.
  if (~member?(#"abstract", modifiers) & ~member?("concrete", modifiers))
    modifiers := add!(modifiers, #"concrete");
  end if;
  // Classes are free by default.
  if (~member?(#"free", modifiers) & ~member?("primary", modifiers))
    modifiers := add!(modifiers, #"free");
  end if;
  // Classes are sealed by default.
  if (~member?(#"sealed", modifiers) & ~member?("open", modifiers))
    modifiers := add!(modifiers, #"sealed");
  end if;
  this.class-spec-modifiers := modifiers;
end method initialize;



/// A useful macro to define the class specs

define macro class-test-definer
  { define ?protocol-name:name class-test ?class-name:name ()
      ?body:body
    end }
    => { define ?protocol-name definition-test ?class-name () ?body end }
end macro class-test-definer;


/// Class spec modeling

define method register-class
    (spec :: <protocol-spec>, name :: <symbol>, binding-function :: <function>)
 => ()
  register-binding(protocol-class-bindings(spec), name, binding-function)
end method register-class;

define method protocol-classes
    (spec :: <protocol-spec>) => (classes :: <table>)
  protocol-bindings(protocol-class-bindings(spec))
end method protocol-classes;

define method protocol-unbound-classes
    (spec :: <protocol-spec>) => (classes :: <sequence>)
  protocol-unbound-bindings(protocol-class-bindings(spec))
end method protocol-unbound-classes;

define method protocol-definition-spec
    (protocol-spec :: <protocol-spec>, class :: <class>)
 => (class-spec :: false-or(<class-spec>))
  element(protocol-classes(protocol-spec), class, default: #f)
end method protocol-definition-spec;

define method protocol-class-superclasses
    (spec :: <protocol-spec>, class :: <class>) => (superclasses :: <sequence>)
  let class-spec = protocol-definition-spec(spec, class);
  class-spec-superclasses(class-spec)
end method protocol-class-superclasses;

define method protocol-class-modifiers
    (spec :: <protocol-spec>, class :: <class>) => (modifiers :: <sequence>)
  let class-spec = protocol-definition-spec(spec, class);
  class-spec-modifiers(class-spec)
end method protocol-class-modifiers;

define method protocol-class-instantiable?
    (spec :: <protocol-spec>, class :: <class>) => (instantiable? :: <boolean>)
  member?(#"instantiable", protocol-class-modifiers(spec, class))
// I deleted the following because it causes tests that initially fail
// because the programmer failed to provide a make-test-instance method
// to pass on subsequent test runs.  -- carlg
//    & begin
//        let info = protocol-class-bindings(spec);
//        ~member?(class, protocol-uninstantiable-classes(info))
//      end
end method protocol-class-instantiable?;

define method protocol-class-abstract?
    (spec :: <protocol-spec>, class :: <class>)
 => (abstract? :: <boolean>)
  member?(#"abstract", protocol-class-modifiers(spec, class))
end method protocol-class-abstract?;

define method do-protocol-classes
    (function :: <function>, spec :: <protocol-spec>, 
     #key superclass :: <class> = <object>)
 => ()
  do-protocol-definitions
    (method (class-spec :: <class-spec>) => ()
       let class = class-spec-class(class-spec);
       if (subtype?(class, superclass))
	 function(class)
       end
     end,
     spec, <class-spec>)
end method do-protocol-classes;


/// Class checking

define method check-protocol-class
    (protocol-spec :: <protocol-spec>, class-spec :: <class-spec>) => ()
  let title = spec-title(class-spec);
  let class = class-spec-class(class-spec);
  with-test-unit (format-to-string("%s tests", title))
    check-instance?(format-to-string("Variable %s is a class", title),
		    <class>, class);
    check-true(format-to-string("Variable %s has the correct superclasses", title),
	       protocol-class-has-correct-superclasses?(protocol-spec, class));
    check-protocol-class-instantiation(protocol-spec, class-spec);
    test-protocol-definition
      (protocol-spec, spec-name(protocol-spec), spec-name(class-spec))
  end
end method check-protocol-class;
    
define function check-protocol-classes
    (protocol-spec :: <protocol-spec>) => ()
  do-protocol-definitions
    (curry(check-protocol-class, protocol-spec), 
     protocol-spec, <class-spec>);
  do(method (class-name :: <string>) => ()
       check-true(format-to-string("The variable %s is a class", class-name), #f)
     end,
     protocol-unbound-classes(protocol-spec));
end function check-protocol-classes;

define method protocol-class-has-correct-superclasses?
    (spec :: <protocol-spec>, class :: <class>) => (correct? :: <boolean>)
  every?(method (superclass :: <class>) => (subtype? :: <boolean>)
           subtype?(class, superclass)
         end,
         protocol-class-superclasses(spec, class))
end method protocol-class-has-correct-superclasses?;


/// Class instantiation checks

define method make-test-instance
    (class :: <class>) => (object)
  make(class)
end method make-test-instance;

define method destroy-test-instance
    (class :: <class>, object :: <object>) => ()
  #f
end method destroy-test-instance;

define method check-protocol-class-instantiation
    (spec :: <protocol-spec>, class-spec :: <class-spec>) => ()
  let class = class-spec-class(class-spec);
  let title = spec-title(class-spec);
  if (protocol-class-instantiable?(spec, class))
    let instance = #f;
    check-instance?(format-to-string("make %s with required arguments", title),
		    class,
		    instance := make-test-instance(class));
    if (instance)
      destroy-test-instance(class, instance)
    else
      let info = protocol-class-bindings(spec);
      add!(protocol-uninstantiable-classes(info), class)
    end
  else
    check-condition
      (format-to-string("make(%s) errors because not instantiable", title),
       <error>,
       begin
	 let instance = make-test-instance(class);
	 destroy-test-instance(class, instance)
       end)
  end
end method check-protocol-class-instantiation;
