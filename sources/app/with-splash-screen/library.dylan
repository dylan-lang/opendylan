Module:    dylan-user
Synopsis:  Launch another app, providing a splash screen until it's ready.
Author:    Hugh Greene
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library with-splash-screen
  use dylan;
  use common-dylan;
  use system;
  use c-ffi;
  use win32-common;
  use win32-kernel;
  use win32-user;
  use win32-gdi;
  use win32-dde;
  use win32-registry;
end library with-splash-screen;

