Module:    dylan-user
Author:    Peter Benson
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library c-ffi-test
  use dylan;
  use common-dylan;
  use c-ffi;
  use testworks;

  export c-ffi-test
end;

define module c-ffi-test
  use common-dylan;
  use dylan-extensions, import: { <byte-character> };
  use testworks;
  use machine-words;
  use c-ffi;
//    exclude: { test-function };
  use simple-io;

  export c-ffi-suite
end;

