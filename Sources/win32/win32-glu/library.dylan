Module:    dylan-user
Synopsis:  Raw interface to Win32 GLU
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-glu
  use functional-dylan;
  use C-FFI;
  use win32-common;
  use win32-gl;
  export win32-glu;
end library win32-glu;

// eof

