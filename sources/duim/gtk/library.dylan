Module:       Dylan-User
Synopsis:     DUIM back-end for GTK
Author:       Andy Armstrong, Scott McKay
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library gtk-duim
  use common-dylan;
  use system;

  use duim-utilities;
  use duim-core;
  use gtk-duim-gadget-panes; //---*** until we've got all native gadgets in

  use c-ffi;

  use glib;
  use gobject;
  use pango;
  use cairo;
  use gdk;
  use gtk;

  export gtk-duim;

end library gtk-duim;
