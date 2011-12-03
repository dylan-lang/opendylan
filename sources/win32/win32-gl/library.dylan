Module:    dylan-user
Synopsis:  Raw Win32 OpenGL interface 
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-gl
  use common-dylan;
  use C-FFI;
  use win32-common;
  export win32-gl;
end library win32-gl;
