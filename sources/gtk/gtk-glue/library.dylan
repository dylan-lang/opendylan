Module:    dylan-user
Author:    Hannes Mehnert, Andreas Bogk
copyright: Original Code is Copyright (c) 2007 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library gtk
  use common-dylan;
  use dylan;
  use c-ffi;
  use gtk-c-ffi;
  // Add any more module exports here.
  export gtk;
end library gtk;
