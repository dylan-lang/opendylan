Module:       dylan-user
Synopsis:     Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module dylan-test-suite
  use dylan-extensions,
    import: { <limited-collection>,
              element-type,
              <hash-state> };
  use common-dylan;
  use format;
  use table-extensions,
    import: { case-insensitive-equal };
  use testworks;
  use testworks-specs;

  // Suites
  export dylan-test-suite;

  // Generics
  export test-collection-class,
         test-condition-class,
         test-number-class;

  // Stifle compiler warnings
end module dylan-test-suite;
