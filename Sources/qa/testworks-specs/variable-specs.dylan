Module:       testworks-specs
Synopsis:     A library for building specification test suites
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Variable specs

define abstract class <abstract-variable-spec> (<definition-spec>)
  constant slot variable-spec-type :: <type>,
    required-init-keyword: type:;
  constant slot variable-spec-getter :: <function>,
    required-init-keyword: getter:;
end class <abstract-variable-spec>;

define class <variable-spec> (<abstract-variable-spec>)
  constant slot variable-spec-setter :: <function>,
    required-init-keyword: setter:;
end class <variable-spec>;

define class <constant-spec> (<abstract-variable-spec>)
end class <constant-spec>;


/// A useful macro to define the class specs

define macro variable-test-definer
  { define ?protocol-name:name variable-test ?variable-name:name ()
      ?body:body
    end }
    => { define ?protocol-name definition-test ?variable-name () ?body end }
end macro variable-test-definer;

define macro constant-test-definer
  { define ?protocol-name:name constant-test ?constant-name:name ()
      ?body:body
    end }
    => { define ?protocol-name definition-test ?constant-name () ?body end }
end macro constant-test-definer;


/// Variable spec modeling

define method register-variable
    (spec :: <protocol-spec>, name :: <symbol>, type :: <type>,
     variable-getter :: <function>, variable-setter :: <function>)
 => ()
  register-definition(spec, name,
		      make(<variable-spec>,
			   name: name,
			   type: type,
			   getter: variable-getter,
			   setter: variable-setter))
end method register-variable;

define method register-constant
    (spec :: <protocol-spec>, name :: <symbol>, type :: <type>,
     constant-getter :: <function>)
 => ()
  register-definition(spec, name,
		      make(<constant-spec>,
			   name: name,
			   type: type,
			   getter: constant-getter))
end method register-constant;


/// Variable testing

define function check-protocol-variable
    (protocol-spec :: <protocol-spec>, variable-spec :: <variable-spec>) => ()
  let title = spec-title(variable-spec);
  with-test-unit (format-to-string("%s tests", title))
    check-instance?(format-to-string("Variable %s has the correct type", title),
		    variable-spec-type(variable-spec),
		    variable-spec-getter(variable-spec)());
    check-true(format-to-string("Variable %s can be set to itself", title),
	       begin
		 let value = variable-spec-getter(variable-spec)();
		 variable-spec-setter(variable-spec)(value) = value
	       end);
    test-protocol-definition
      (protocol-spec, spec-name(protocol-spec), spec-name(variable-spec))
  end
end function check-protocol-variable;

define function check-protocol-variables
    (protocol-spec :: <protocol-spec>) => ()
  do-protocol-definitions
    (curry(check-protocol-variable, protocol-spec),
     protocol-spec, <variable-spec>)
end function check-protocol-variables;

define function check-protocol-constant
    (protocol-spec :: <protocol-spec>, constant :: <constant-spec>) => ()
  let title = spec-title(constant);
  with-test-unit (format-to-string("%s tests", title))
    check-instance?(format-to-string("Constant %s has the correct type", title),
		    variable-spec-type(constant),
		    variable-spec-getter(constant)());
    test-protocol-definition
      (protocol-spec, spec-name(protocol-spec), spec-name(constant))
  end
end function check-protocol-constant;

define function check-protocol-constants
    (protocol-spec :: <protocol-spec>) => ()
  do-protocol-definitions
    (curry(check-protocol-constant, protocol-spec),
     protocol-spec, <constant-spec>)
end function check-protocol-constants;
