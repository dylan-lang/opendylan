Module:    dylan-user
Library:   scribble
Author:    Scott McKay
Synopsis:  Simple scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      Functional Objects Library Public License Version 1.0
Dual-license: GNU Lesser General Public License
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library mini-scribble
  use functional-dylan;
  use mini-duim;

  export scribble;
end library mini-scribble;

define module scribble
  use functional-dylan;
  use mini-duim;

  export <scribble-pane>,
         <scribble-frame>,
         draw-segment,
         contained-scribble,
         contained-button,
         attach-scribble,
         scribble;
end module scribble;
