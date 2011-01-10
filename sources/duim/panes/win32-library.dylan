Module:       Dylan-User
Synopsis:     DUIM concrete gadget panes, for Win32
Author:       Scott McKay, Andy Armstrong
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library duim-gadget-panes
  use dylan;

  use duim-utilities;
  use duim-geometry;
  use duim-DCs;
  use duim-sheets;
  use duim-graphics;  
  use duim-layouts;
  use duim-gadgets;
  use duim-frames;

  export duim-gadget-panes;
  export duim-gadget-panes-internals;
end library duim-gadget-panes;
