Module:    dylan-user
Author:    Peter Benson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library c-ffi-test
  use functional-dylan;
  use c-ffi;
  use testworks;

  export c-ffi-test
end;

define module c-ffi-test
  use functional-dylan;
  use testworks;
  use machine-words;
  use c-ffi;
//    exclude: { test-function };
  use simple-format;

  export c-ffi-suite
end;

