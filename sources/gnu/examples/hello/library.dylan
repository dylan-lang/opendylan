Module:       dylan-user
Author:       Scott McKay
Synopsis:     First "Hello World" example from GTK Tutorial
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library gtk-hello-world
  use functional-dylan;

  use glib;
  use gdk;
  use gtk;
  
  export gtk-hello-world;
end library gtk-hello-world;

define module gtk-hello-world
  use functional-dylan;

  use glib;
  use gdk;
  use gtk;
end module gtk-hello-world;
