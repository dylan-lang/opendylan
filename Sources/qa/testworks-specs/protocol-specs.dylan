Module:       testworks-specs
Synopsis:     A library for building specification test suites
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Protocol bindings modeling

define class <protocol-bindings-info> (<object>)
  constant slot protocol-binding-functions = make(<table>);
  slot %unbound-bindings :: false-or(<sequence>) = #f;
  slot %definitions :: false-or(<table>) = #f;
end class <protocol-bindings-info>;

define class <protocol-class-bindings-info> (<protocol-bindings-info>)
  constant slot protocol-uninstantiable-classes = make(<stretchy-vector>);
end class <protocol-class-bindings-info>;

define method evaluate-bindings
    (info :: <protocol-bindings-info>)
 => (bindings :: <table>, unbound-bindings :: <sequence>)
  let table = make(<table>);
  let unbound-bindings = make(<stretchy-vector>);
  for (function keyed-by name in info.protocol-binding-functions)
    let (value, spec)
      = block ()
	  function();
	exception (<error>)
          add!(unbound-bindings, as-lowercase(as(<byte-string>, name)));
          #f
	end;
    if (value) 
      table[value] := spec;
    end
  end;
  values(table, unbound-bindings)
end method evaluate-bindings;

define method update-bindings
    (info :: <protocol-bindings-info>) => ()
  let (bindings, unbound-bindings) = evaluate-bindings(info);
  info.%unbound-bindings := unbound-bindings;
  info.%definitions := bindings
end method update-bindings;

define method protocol-bindings
    (info :: <protocol-bindings-info>) => (bindings :: <table>)
  info.%definitions
    | begin
        update-bindings(info);
        info.%definitions
      end
end method protocol-bindings;

define method protocol-unbound-bindings
    (info :: <protocol-bindings-info>) => (bindings :: <sequence>)
  info.%unbound-bindings
    | begin
        update-bindings(info);
        info.%unbound-bindings
      end
end method protocol-unbound-bindings;

define method register-binding
    (info :: <protocol-bindings-info>, name :: <symbol>, 
     binding-function :: <function>)
 => ()
  let table = protocol-binding-functions(info);
  table[name] := binding-function;
  // Clear the caches so that they get recomputed
  info.%unbound-bindings := #f;
  info.%definitions := #f;
end method register-binding;


/// Protocol specs modeling

define class <protocol-spec> (<spec>)
  constant slot protocol-class-bindings = make(<protocol-class-bindings-info>);
  constant slot protocol-function-bindings = make(<protocol-bindings-info>);
  constant slot %definitions :: <table> = make(<table>);
end class <protocol-spec>;

define function protocol-definitions
    (spec :: <protocol-spec>) => (definitions :: <table>)
  let definitions = spec.%definitions;
  let class-bindings = protocol-class-bindings(spec);
  unless (class-bindings.%definitions)
    let class-definitions = protocol-bindings(class-bindings);
    do(method (definition-spec)
	 definitions[spec-name(definition-spec)] := definition-spec
       end,
       class-definitions)
  end;
  let function-bindings = protocol-function-bindings(spec);
  unless (function-bindings.%definitions)
    let function-definitions = protocol-bindings(function-bindings);
    do(method (definition-spec)
	 definitions[spec-name(definition-spec)] := definition-spec
       end,
       function-definitions)
  end;
  definitions
end function protocol-definitions;

define method protocol-definition-spec
    (protocol-spec :: <protocol-spec>, name :: <symbol>)
 => (definition :: false-or(<definition-spec>))
  let definitions = protocol-definitions(protocol-spec);
  element(definitions, name, default: #f)
end method protocol-definition-spec;

define function do-protocol-definitions
    (function :: <function>, spec :: <protocol-spec>, type :: <type>) => ()
  do(method (binding)
       when (instance?(binding, type))
	 function(binding)
       end
     end,
     protocol-definitions(spec))
end function do-protocol-definitions;

define method register-definition
    (spec :: <protocol-spec>, name :: <symbol>, 
     definition :: <definition-spec>)
 => ()
  let table = spec.%definitions;
  table[name] := definition
end method register-definition;


/// A useful macro to define protocol specs

define macro protocol-spec-definer
  { define protocol-spec ?protocol-name:name (?options:*)
      ?specs:*
    end}
    => { define protocol-spec-constant ?protocol-name (?options)
         end;
         define protocol-spec-bindings "$" ## ?protocol-name ## "-protocol-spec" (?options)
           ?specs
         end;
         define protocol-spec-suite ?protocol-name => "$" ## ?protocol-name ## "-protocol-spec";
         }
end macro protocol-spec-definer;

define macro protocol-spec-constant-definer
  { define protocol-spec-constant ?protocol-name:name (?options:*) end}
    => { define constant "$" ## ?protocol-name ## "-protocol-spec"
           = make(<protocol-spec>, 
                  name: ?#"protocol-name",
                  ?options) }
end macro protocol-spec-constant-definer;

define macro protocol-spec-bindings-definer
  { define protocol-spec-bindings ?protocol-constant:name (?options:*)
    end }
    => { }
  { define protocol-spec-bindings ?protocol-constant:name (?options:*)
      ?modifiers:* class ?class-name:name (?superclasses:*);
      ?more-specs:*
    end }
    => { register-class
	  (?protocol-constant,
	   ?#"class-name",
	   method ()
	     values(?class-name,
		    make(<class-spec>,
			 name: ?#"class-name",
			 class: ?class-name,
			 superclasses: vector(?superclasses),
			 modifiers: vector(?modifiers)))
	   end);
         define protocol-spec-bindings ?protocol-constant (?options)
           ?more-specs
         end; }
  { define protocol-spec-bindings ?protocol-constant:name (?options:*)
      ?modifiers:* function ?function-name:name (?parameters:*) => (?results:*);
      ?more-specs:*
    end }
    => { register-function
	  (?protocol-constant,
	   ?#"function-name",
	   method ()
	     values(?function-name,
		    make(<function-spec>,
			 name: ?#"function-name",
			 function: ?function-name,
			 parameters: vector(?parameters),
			 results:    vector(?results),
			 modifiers: vector(?modifiers)))
	   end);
         define protocol-spec-bindings ?protocol-constant (?options) 
           ?more-specs
         end; }
  { define protocol-spec-bindings ?protocol-constant:name (?options:*)
      ?modifiers:* generic-function ?function-name:name (?parameters:*) => (?results:*);
      ?more-specs:*
    end }
    => { register-function
	  (?protocol-constant,
	   ?#"function-name",
	   method ()
	     values(?function-name,
		    make(<function-spec>,
			 name: ?#"function-name",
			 function: ?function-name,
			 parameters: vector(?parameters),
			 results:    vector(?results),
			 modifiers: vector(#"generic", ?modifiers)))
	   end);
         define protocol-spec-bindings ?protocol-constant (?options)
           ?more-specs
         end; }
  { define protocol-spec-bindings ?protocol-constant:name (?options:*)
      ?modifiers:* variable ?variable-name:name :: ?type:expression;
      ?more-specs:*
    end }
    => { register-variable
	   (?protocol-constant,
	    ?#"variable-name",
	    ?type,
	    method () => (value :: ?type)
	      ?variable-name
	    end,
	    method (value :: ?type) => (value :: ?type)
	      ?variable-name := value
	    end);
         define protocol-spec-bindings ?protocol-constant (?options) 
           ?more-specs
         end; }
  { define protocol-spec-bindings ?protocol-constant:name (?options:*)
      ?modifiers:* constant ?constant-name:name :: ?type:expression;
      ?more-specs:*
    end }
    => { register-constant
	   (?protocol-constant,
	    ?#"constant-name",
	    ?type,
	    method () ?constant-name end);
         define protocol-spec-bindings ?protocol-constant (?options) 
           ?more-specs
         end; }
  { define protocol-spec-bindings ?protocol-constant:name (?options:*)
      ?modifiers:* macro-test ?macro-name:name;
      ?more-specs:*
    end }
    => { register-macro(?protocol-constant, ?#"macro-name");
         define protocol-spec-bindings ?protocol-constant (?options)
           ?more-specs
         end; }
 modifiers:
  { }
    => { }
  { ?modifier:name ... }
    => { ?#"modifier", ... }
end macro protocol-spec-bindings-definer;

define macro protocol-spec-suite-definer
  { define protocol-spec-suite ?protocol-name:name => ?spec:name; }
    => { define test ?protocol-name ## "-protocol-classes-test" ()
           check-protocol-classes(?spec)
         end;
         define test ?protocol-name ## "-protocol-functions-test" ()
           check-protocol-functions(?spec)
         end;
         define test ?protocol-name ## "-protocol-variables-test" ()
           check-protocol-variables(?spec)
         end;
         define test ?protocol-name ## "-protocol-constants-test" ()
           check-protocol-constants(?spec)
         end;
         define test ?protocol-name ## "-protocol-macros-test" ()
           check-protocol-macros(?spec)
         end;
         define suite ?protocol-name ## "-protocol-test-suite" ()
           test ?protocol-name ## "-protocol-constants-test";
           test ?protocol-name ## "-protocol-variables-test";
           test ?protocol-name ## "-protocol-classes-test";
           test ?protocol-name ## "-protocol-functions-test";
           test ?protocol-name ## "-protocol-macros-test";
	 end }
end macro protocol-spec-suite-definer;


/// A useful macro to define a definition's test function

define open generic test-protocol-definition
    (protocol :: <protocol-spec>, protocol-name :: <symbol>, 
     definition-name :: <symbol>)
 => ();

define method test-protocol-definition
    (spec :: <protocol-spec>, protocol-name :: <symbol>, 
     definition-name :: <symbol>)
 => ()
  ignore(protocol-name);
  let definition-spec = protocol-definition-spec(spec, definition-name);
  assert(definition-spec,
	 "Attempting to test definition %s which is not part of protocol %s",
	 definition-name, protocol-name);
  let tested?
    = if (instance?(definition-spec, <class-spec>))
	let class = class-spec-class(definition-spec);
	let test-function = class-test-function(class);
	if (test-function)
          let instantiable? = protocol-class-instantiable?(spec, class);
          let abstract? = protocol-class-abstract?(spec, class);
	  test-function(class, 
                        name: spec-title(definition-spec),
                        abstract?: abstract?,
                        instantiable?: instantiable?);
	  #t
	end
      end;
  unless (tested?)
    cerror("Continue past this testing unit",
	   "No test function provided for definition %s", 
	  spec-title(definition-spec))
  end
end method test-protocol-definition;

define macro definition-test-definer
  { define ?protocol-name:name definition-test ?definition-name:name ()
      ?body:body
    end }
    => { define sideways method test-protocol-definition
	     (protocol :: <protocol-spec>,
              protocol-name == ?#"protocol-name",
	      definition    == ?#"definition-name")
	  => ()
	   ?body
	 end method test-protocol-definition }
end macro definition-test-definer;
