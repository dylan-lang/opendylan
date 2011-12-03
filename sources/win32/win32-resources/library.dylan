Module:    dylan-user
Synopsis:  Windows resource decoding
Author:    Roman Budzianowski, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-resources
  use common-dylan;
  use collections;
  use C-FFI;
  use win32-common;
  use win32-user;
  use win32-gdi;
  use win32-kernel;

  export win32-resources;
end library win32-resources;
