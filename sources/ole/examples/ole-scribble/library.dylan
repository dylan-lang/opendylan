Module:    dylan-user
Author:    Scott McKay and David Gray
Synopsis:  Simple OLE scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library ole-scribble
  use dylan;
  use common-dylan;
  use duim;
  use duim-ole-server;
  use dood;

  export scribble;
end library ole-scribble;

define module scribble
  use dylan;
  use common-dylan;
  use duim;
  use duim-ole-server;
  use dood;

  export <scribble-pane>,
         <scribble-frame>,
         scribble;
end module scribble;
