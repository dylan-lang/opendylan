Module:       testworks-specs
Synopsis:     A library for building specification test suites
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Function specs

define class <function-spec> (<definition-spec>)
  constant slot function-spec-function :: <function>,
    required-init-keyword: function:;
  constant slot function-spec-modifiers :: <sequence> = #[],
    init-keyword: modifiers:;
  //---*** Not used yet...
  // constant slot function-spec-parameters :: <sequence> = #[],
  //   init-keyword: parameters:;
  // constant slot function-spec-results :: <sequence> = #[],
  //   init-keyword: results:;
end class <function-spec>;


/// A useful macro to define the function specs

define macro function-test-definer
  { define ?protocol-name:name function-test ?function-name:name ()
      ?body:body
    end }
    => { define ?protocol-name definition-test ?function-name () ?body end }
end macro function-test-definer;


/// Function spec modeling

define method register-function
    (spec :: <protocol-spec>, name :: <symbol>, binding-function :: <function>)
 => ()
  register-binding(protocol-function-bindings(spec), name, binding-function)
end method register-function;

define method protocol-functions
    (spec :: <protocol-spec>) => (classes :: <table>)
  protocol-bindings(protocol-function-bindings(spec))
end method protocol-functions;

define method protocol-unbound-functions
    (spec :: <protocol-spec>) => (functions :: <sequence>)
  protocol-unbound-bindings(protocol-function-bindings(spec))
end method protocol-unbound-functions;

define method protocol-definition-spec
    (protocol-spec :: <protocol-spec>, function :: <function>)
 => (function-spec :: false-or(<function-spec>))
  element(protocol-functions(protocol-spec), function, default: #f)
end method protocol-definition-spec;

define method protocol-function-modifiers
    (spec :: <protocol-spec>, function :: <function>)
 => (modifiers :: <sequence>)
  let function-spec = protocol-definition-spec(spec, function);
  function-spec-modifiers(function-spec)
end method protocol-function-modifiers;

/*--- Not used yet
define method protocol-function-parameters
    (spec :: <protocol-spec>, function :: <function>)
 => (parameters :: <sequence>)
  let function-spec = protocol-definition-spec(spec, function);
  function-spec-parameters(function-spec)
end method protocol-function-parameters;

define method protocol-function-results
    (spec :: <protocol-spec>, function :: <function>)
 => (results :: <sequence>)
  let function-spec = protocol-definition-spec(spec, function);
  function-spec-results(function-spec)
end method protocol-function-results;
*/

define method protocol-function-generic?
    (spec :: <protocol-spec>, function :: <function>)
 => (generic? :: <boolean>)
  member?(#"generic", protocol-function-modifiers(spec, function))
end method protocol-function-generic?;

define function protocol-function-type
    (protocol-spec :: <protocol-spec>, function :: <function>)
 => (type :: <type>, type-name :: <string>)
  if (protocol-function-generic?(protocol-spec, function))
    values(<generic-function>, "generic-function")
  else
    values(<function>, "function")
  end
end function protocol-function-type;

// Yes this check name is verbose, but it aids in debugging testworks-specs tests.
define function protocol-function-check-name
    (function-name :: <string>, type-name :: <string>)
 => (check-name :: <string>)
  format-to-string("Variable %s is a %s and all of its specializer types are bound",
		   function-name, type-name)
end function protocol-function-check-name;

define function check-protocol-function
    (protocol-spec :: <protocol-spec>, function-spec :: <function-spec>)
 => ()
  let title = spec-title(function-spec);
  let function = function-spec-function(function-spec);
  with-test-unit (format-to-string("%s tests", title))
    let (type, type-name) = protocol-function-type(protocol-spec, function);
    check-instance?(protocol-function-check-name(title, type-name),
		    type, function);
    test-protocol-definition
      (protocol-spec, spec-name(protocol-spec), spec-name(function-spec))
  end
end function check-protocol-function;

define function check-protocol-functions
    (protocol-spec :: <protocol-spec>) => ()
  do-protocol-definitions
    (curry(check-protocol-function, protocol-spec),
     protocol-spec, <function-spec>);
  do(method (function-name)
       // This function is unbound; its type can't be determined so
       // just say it's a "function".
       let name = protocol-function-check-name(function-name, "function");
       check-true(name, #f)
     end,
     protocol-unbound-functions(protocol-spec))
end function check-protocol-functions;
