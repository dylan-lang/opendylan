Module:       Dylan-User
Synopsis:     DUIM back-end for GTK
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library gtk-duim
  use common-dylan;

  use duim-utilities;
  use duim-core;
  use gtk-duim-gadget-panes;	//---*** until we've got all native gadgets in

  use C-FFI;
  use Glib;
  use Gdk;
  use Gtk;

  export gtk-duim;
end library gtk-duim;
