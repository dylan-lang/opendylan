module: dylan-user
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library url-test-suite
  use functional-dylan;
  use testworks;

  use network-conditions;
  use url;
  export url-test-suite;
end library;

define module url-test-suite
  use functional-dylan;
  use testworks;

  use network-conditions;
  use url;
  
  export url-test-suite;
end module;
