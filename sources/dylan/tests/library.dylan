Module:       dylan-user
Synopsis:     A Dylan test suite
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library dylan-test-suite
  use dylan;		//---*** just for dylan-extensions
  use common-dylan;
  use testworks;
  use testworks-specs;

  export dylan-test-suite;
end library dylan-test-suite;
