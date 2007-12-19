Module:    dylan-user
Author:    Hannes Mehnert, Andreas Bogk
Copyright: (C) 2007,.  All rights reserved.

define library gtk-c-ffi
  use common-dylan;
  use c-ffi;
  use io;

  // Add any more module exports here.
  export gtk-internal;
end library gtk-c-ffi;
