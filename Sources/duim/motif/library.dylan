Module:    Dylan-User
Synopsis:  DUIM back-end for Motif
Author:    Scott McKay, Stuart Croy
	   Based on work by John Aspinall and Richard Billington
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library motif-duim
  use common-dylan;

  use duim-utilities;
  use duim-core;
  use duim-gadget-panes;	//---*** until we've got all native gadgets in

  use C-FFI;
  use Xlib;
  use Xt;
  use Motif;

  export motif-duim;
end library motif-duim;
