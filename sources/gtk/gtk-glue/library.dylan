Module:    dylan-user
Author:    Hannes Mehnert, Andreas Bogk
Copyright: (C) 2007,.  All rights reserved.

define library gtk
  use common-dylan;
  use dylan;
  use c-ffi;
  use gtk-c-ffi;
  // Add any more module exports here.
  export gtk;
end library gtk;
