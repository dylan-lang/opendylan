Module:    dylan-user
Author:    Peter Benson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library c-ffi-test-suite
  use dylan;
  use common-dylan;
  use c-ffi;
  use big-integers;
  use testworks;

  export c-ffi-test-suite
end;

define module c-ffi-test-suite
  use common-dylan;
  use testworks;
  use machine-words;
  use c-ffi;

  export c-ffi-test-suite;
end;

