Module:    dylan-user
Library:   ole-scribble
Author:    Scott McKay and David Gray
Synopsis:  Simple scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library OLE-scribble
  use Dylan;
  use functional-extensions;
  use mini-DUIM;
  use DUIM-OLE-server;

  export scribble;
end library OLE-scribble;

define module scribble
  use Dylan;
  use functional-extensions;

  use mini-DUIM;
  use DUIM-OLE-server;

  export

   contained-button,
   contained-scribble,
   attach-scribble,
   scribble,
   window-menu,
   clear-button,
   surface,
   window-menu-pane-setter,
   window-menu-pane,
   clear-button-pane-setter,
   clear-button-pane,
   surface-pane-setter,
   surface-pane,
   clear-surface,
   add-scribble-segment,
   draw-segment,
   scribble-segments-setter,
   scribble-segments,
   scribble-segment-setter,
   scribble-segment,
   run-scribble,
   $validation-code,
   $OLE-class-ID,
   <scribble-frame>,
   <scribble-pane>;

end module scribble;
