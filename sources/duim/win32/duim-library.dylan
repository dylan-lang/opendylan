Module:    Dylan-User
Synopsis:  DUIM core + Win32 back-end
Author:    Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim
  use dylan;

  // Use the DUIM core and re-export all its modules, then add
  // the Win32 back-end and re-export all its modules as well
  use duim-core,  export: all;
  use win32-duim, export: all;
end library duim;
