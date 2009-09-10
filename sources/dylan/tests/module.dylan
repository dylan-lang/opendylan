Module:       dylan-user
Synopsis:     Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dylan-test-suite
  use dylan-extensions,
    import: { <limited-collection>,
              element-type,
              <hash-state> };
  use common-dylan;
  use testworks;
  use testworks-specs;

  // Suites
  export dylan-test-suite;

  // Generics
  export test-collection-class,
         test-condition-class,
         test-number-class;
end module dylan-test-suite;
