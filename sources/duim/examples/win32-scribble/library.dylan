Module:       dylan-user
Author:       Scott McKay
Synopsis:     Win32 version of the DUIM scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-scribble
  use functional-dylan;
  use duim;

  use c-ffi;
  use win32-common;
  use win32-user;
  use win32-gdi;
  use win32-kernel;
  use win32-dialog;

  export win32-scribble;
end library win32-scribble;

define module win32-scribble
  use functional-dylan;
  use duim;
  use win32-duim;
  use duim-internals,
    import: { sheet-mirror };

  use C-FFI;
  use win32-common,
    rename: { <point> => w/<point> };
  use win32-user;
  use win32-gdi,
    rename: { <PATTERN> => w/<PATTERN> };
  use win32-kernel,
    rename: { Beep  => w/Beep,
	      Sleep => w/Sleep };
  use win32-dialog;

  export <scribble-pane>,
         <scribble-frame>,
         scribble;
end module win32-scribble;
