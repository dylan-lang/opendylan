Module:       dylan-user
Synopsis:     A Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dylan-test-suite
  use dylan;    //---*** just for dylan-extensions
  use common-dylan;
  use collections,
    import: { table-extensions };
  use testworks;
  use testworks-specs;

  export dylan-test-suite;
end library dylan-test-suite;
