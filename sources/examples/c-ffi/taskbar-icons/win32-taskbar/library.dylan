Module:   dylan-user
Synopsis: Raw interface to win32 taskbar management
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-taskbar
  use win32-common;
  use win32-user;
  use c-ffi;
  use collections;
  use functional-dylan;

  export win32-taskbar;
end library win32-taskbar;
