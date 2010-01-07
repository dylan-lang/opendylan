Module:    dylan-user
Synopsis:  Raw Win32 OpenGL interface 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-gl
  use functional-dylan;
  use C-FFI;
  use win32-common;
  export win32-gl;
end library win32-gl;
