Module:    Dylan-User
Synopsis:  DUIM back-end for Microsoft Windows
Author:    David Gray, Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library win32-duim
  use functional-dylan;

  use duim-utilities;
  use duim-core;
  use duim-gadget-panes;	//---*** until we've got all native gadgets in

  use C-FFI;

  export win32-duim;
end library win32-duim;
