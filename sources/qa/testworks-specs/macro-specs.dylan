Module:       testworks-specs
Synopsis:     A library for building specification test suites
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

/// Macro specs

define class <macro-spec> (<definition-spec>)
end class <macro-spec>;


/// A useful macro to define a macro test

define macro macro-test-definer
  { define ?protocol-name:name macro-test ?macro-name:name ()
      ?body:body
    end }
    => { define ?protocol-name definition-test ?macro-name () ?body end }
end macro macro-test-definer;


/// Macro spec modeling

define method register-macro
    (spec :: <protocol-spec>, name :: <symbol>)
 => ()
  register-definition(spec, name, make(<macro-spec>, name: name))
end method register-macro;


/// Macro testing

define method check-protocol-macro
    (protocol-spec :: <protocol-spec>, macro-spec :: <macro-spec>) => ()
  let title = spec-title(macro-spec);
  with-test-unit (format-to-string("%s tests", title))
    test-protocol-definition
      (protocol-spec, spec-name(protocol-spec), spec-name(macro-spec))
  end
end method check-protocol-macro;

define method check-protocol-macros
    (protocol-spec :: <protocol-spec>) => ()
  do-protocol-definitions
    (curry(check-protocol-macro, protocol-spec),
     protocol-spec, <macro-spec>)
end method check-protocol-macros;
