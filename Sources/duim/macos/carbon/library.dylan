Module:       Dylan-User
Synopsis:     DUIM back-end for the Macintosh
Author:       Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library carbon-duim
  use common-dylan;

  use duim-utilities;
  use duim-core;
  use duim-gadget-panes;	//---*** until we've got all native gadgets in

  use C-FFI;
  use carbon-interface;		//---*** temporary

  export carbon-duim;
end library carbon-duim;
