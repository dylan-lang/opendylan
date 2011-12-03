Module:   dylan-user
Synopsis: A library abstracting persistent status icon display
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library status-icons
  use common-dylan;
  use collections;
  use win32-taskbar;
  use win32-user;
  use win32-common;
  use c-ffi;

  export status-icons;
end library status-icons;
