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
              <hash-state>,
              <limited-integer>,
              limited-integer-min,
              limited-integer-max };
  use common-dylan;
  use common-dylan-test-utilities;
  use format;
  use simple-random;
  use table-extensions,
    import: { case-insensitive-equal };
  use testworks;

  export dylan-test-suite;
end module;
