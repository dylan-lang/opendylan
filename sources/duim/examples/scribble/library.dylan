Module:       dylan-user
Author:       Scott McKay
Synopsis:     Simple scribble application
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library scribble
  use common-dylan;
  use duim;

  export scribble;
end library scribble;

define module scribble
  use common-dylan;
  use duim;

  export <scribble-pane>,
         <scribble-frame>,
         scribble;
end module scribble;
