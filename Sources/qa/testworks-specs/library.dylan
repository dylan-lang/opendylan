Module:       dylan-user
Synopsis:     A library for building specification test suites
Author:	      Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library testworks-specs
  use common-dylan;
  use testworks;

  export testworks-specs;
end library testworks-specs;

define module testworks-specs
  use common-dylan;
  use testworks;

  // The macros
  export \library-spec-definer,
         \module-spec-definer,
         \protocol-spec-definer,
         \definition-test-definer,
         \constant-test-definer,
         \variable-test-definer,
         \class-test-definer,
         \function-test-definer,
         \macro-test-definer;

  // The classes
  export <spec>,
         <protocol-spec>,
         <definition-spec>,
         <constant-spec>,
         <variable-spec>,
         <class-spec>,
         <function-spec>,
         <macro-spec>;

  // Useful accessors
  export spec-name,
         spec-title,
         protocol-definition-spec;

  // The test functions
  export make-test-instance,
         destroy-test-instance,
         test-protocol-definition,
         class-test-function,
         do-protocol-classes,
         check-protocol-constants,
         check-protocol-variables,
         check-protocol-classes,
         check-protocol-functions,
         check-protocol-macros;

  // Class handling functions
  export do-protocol-classes,
         protocol-class-abstract?,
         protocol-class-instantiable?;

  //---*** Hygiene glitches
  export \protocol-spec-constant-definer,
         \protocol-spec-bindings-definer,
         \protocol-spec-suite-definer,
         \module-spec-protocol-definer,
         \module-spec-suite-definer,         
         register-constant,
         register-variable,
         register-class,   
         register-function,
         register-macro;
end module testworks-specs;
