Module:       dylan-user
Author:       Andy Armstrong
Synopsis:     Windows viewer
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define module windows-viewer
  use functional-dylan;
  use threads;
  use streams;
  use format;
  use format-out;

  use c-ffi;
  use win32-common;
  use win32-kernel,
    exclude: { beep,
               sleep };
  use win32-user;
  use win32-dde,
    exclude: { $LT-LOWEST-LATENCY,
	       $LT-DONT-CARE };

  use duim,
    exclude: { <point> };

  use windows-hook;

  export <windows-viewer>,
         start-windows-viewer;
end module windows-viewer;
