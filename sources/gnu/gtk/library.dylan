module:    Dylan-user	
Synopsis:  FFI declarations translated from GTK header files.
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library GTK
  use GLib,        export: all;
  use GDK,         export: all;
  use GTK-Common,  export: all;
  use GTK-Widgets, export: all;
  export GTK;
end library GTK;

define module GTK
  use GLib,        export: all;
  use GDK,         export: all;
  use GTK-Common,  export: all;
  use GTK-Widgets, export: all;
end module GTK;
