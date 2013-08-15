Module:       dylan-user
Author:       Carl Gay
Synopsis:     The game of Life
Copyright:    Original Code is Copyright (c) 1995-2004 Functional Objects, Inc.
              All rights reserved.
License:      See License.txt in this distribution for details.
Warranty:     Distributed WITHOUT WARRANTY OF ANY KIND

define library life
  use common-dylan;
  use system;
  use duim;

  export life;
end library life;


define module life
  use common-dylan;
  use simple-random;            // exported from common-dylan
  use simple-profiling;
  use operating-system;         // exported from system
  use threads;

  use duim;
  use duim-internals,
    import: { do-command-menu-gadgets };

  export <life-frame>,
         life;
end module life;
