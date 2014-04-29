Module:       dylan-user
Synopsis:     DUIM test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

// Implementation module
define module duim-test-suite
  use common-dylan,
    exclude: { position };
  use simple-format;
  use threads;

  use testworks;
  use testworks-specs;

  use duim-internals;
  use duim-gadget-panes-internals;
  use duim-extended-geometry;

  export duim-suite,
         duim-test-suite;
end module duim-test-suite;
