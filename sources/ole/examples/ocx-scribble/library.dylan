Module:    dylan-user
Author:    Scott McKay and David Gray
Synopsis:  Simple OLE control (OCX) scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library ocx-scribble
  use functional-dylan;
  use duim;
  use duim-ole-control;

  export scribble;
end library ocx-scribble;

define module scribble
  use functional-dylan;
  use duim;
  use duim-ole-control;

  export <scribble-pane>,
         <scribble-frame>;
end module scribble;
