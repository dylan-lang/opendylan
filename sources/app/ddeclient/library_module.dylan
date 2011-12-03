Module:    dylan-user
Synopsis:  A simple program to send a single synchronous DDE client request.
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library ddeclient
  use common-dylan;
  use system;
  use io;

  use c-ffi;
  use win32-common;
  use win32-dde;
end library;

define module ddeclient
  use common-dylan;
  use operating-system;
  use format-out;

  use c-ffi;
  use win32-common;
  use win32-dde;
end module;
