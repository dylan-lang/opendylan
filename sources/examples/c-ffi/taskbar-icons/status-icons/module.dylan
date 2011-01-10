Module:   dylan-user
Synopsis: A library abstracting persistent status icon display
Author:   Keith Playford
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module status-icons
  use win32-taskbar;
  use win32-user;
  use win32-common;
  use c-ffi;
  use threads;
  use table-extensions;
  use machine-words;
  use finalization;
  use functional-dylan;
  use simple-format;
  use simple-random;

  export
    display-status-ok,
    display-status-warning,
    display-status-error,
    stop-status-display;

end module status-icons;
