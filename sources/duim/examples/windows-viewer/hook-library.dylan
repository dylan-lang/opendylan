Module:    dylan-user
Author:    Andy Armstrong
Synopsis:  Windows hook
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library windows-hook
  use functional-dylan;

  use c-ffi;
  use win32-common;
  use win32-kernel;
  use win32-user;

  export windows-hook;
end library windows-hook;
