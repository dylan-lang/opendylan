Module:    dylan-user
Author:    Hannes Mehnert, Andreas Bogk
copyright: Original Code is Copyright (c) 2007 Dylan Hackers;
           All rights reversed.
License:   See License.txt in this distribution for details.
Warranty:  Distributed WITHOUT WARRANTY OF ANY KIND

define library gtk-c-ffi
  use common-dylan;
  use c-ffi;
  use io;

  // Add any more module exports here.
  export gtk-internal;
end library gtk-c-ffi;
